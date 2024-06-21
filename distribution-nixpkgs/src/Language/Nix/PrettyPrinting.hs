{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}

-- | Internal pretty-printing helpers for Nix expressions.

module Language.Nix.PrettyPrinting
  ( onlyIf
  , toAscList, toAscListSortedOn
  , setattr
  , listattr, listattrDoc
  , boolattr
  , condTreeAttr
  , attr
  , string
  , funargs
  -- * Re-exports from other modules
  , module Text.PrettyPrint.HughesPJClass
  )
  where

-- Avoid name clash with Prelude.<> exported by post-SMP versions of base.
#if MIN_VERSION_base(4,11,0)
import Prelude hiding ( (<>) )
#endif
import Data.Char
import Data.Function
import Data.List (sortBy)
import Data.Set ( Set )
import qualified Data.Set as Set
import Distribution.PackageDescription (CondBranch(..), Condition (..), CondTree(CondNode), ConfVar (..), Dependency, unFlagName)
import "pretty" Text.PrettyPrint.HughesPJClass
import Language.Nix.Binding (Binding, localName)
import Control.Lens
import Language.Nix (ident)
import Distribution.System (OS(..), Arch (..))
import Distribution.Compiler (CompilerFlavor(..))

attr :: String -> Doc -> Doc
attr n v = text n <+> equals <+> v <> semi

onlyIf :: Bool -> Doc -> Doc
onlyIf b d = if b then d else empty

boolattr :: String -> Bool -> Bool -> Doc
boolattr n p v = if p then attr n (bool v) else empty

listattrDoc :: String -> Doc -> [Doc] -> Doc
listattrDoc n prefix vs = onlyIf (not (null vs)) $
  sep [ text n <+> equals <+> prefix <+> lbrack,
        nest 2 $ fsep vs,
        rbrack <> semi
      ]

listattr :: String -> Doc -> [String] -> Doc
listattr n p = listattrDoc n p . map text

setattr :: String -> Doc -> Set String -> Doc
setattr name prefix set = listattrDoc name prefix
  $ map text $ toAscList set

toAscListSortedOn :: (a -> String) -> Set a -> [a]
toAscListSortedOn f = sortBy (compare `on` map toLower . f) . Set.toList

toAscList :: Set String -> [String]
toAscList = toAscListSortedOn id

bool :: Bool -> Doc
bool True  = text "true"
bool False = text "false"

string :: String -> Doc
string = doubleQuotes . quoteString

quoteString :: String -> Doc
quoteString []          = mempty
quoteString ['\\']      = text "\\\\"
quoteString ('\\':x:xs)
  | x `elem` "\"rn$\\t" = text ['\\',x] <> quoteString xs
  | otherwise           = text "\\\\" <> quoteString (x:xs)
quoteString (x:xs)      = char x <> quoteString xs

prepunctuate :: Doc -> [Doc] -> [Doc]
prepunctuate _ []     = []
prepunctuate p (d:ds) = d : map (p <>) ds

funargs :: [Doc] -> Doc
funargs xs = sep [
               lbrace <+> fcat (prepunctuate (comma <> text " ") $ map (nest 2) xs),
               rbrace <> colon
             ]

condTreeAttr :: String -> CondTree ConfVar [Dependency] (Set Binding) -> Doc
condTreeAttr n = rootAttr . tree
  where
    indented header footer xs =
      sep [ header,
            nest 2 xs,
            footer
          ]

    rootAttr vs = onlyIf (not (null vs)) $
      indented (text n <+> equals) mempty $
        lists True vs

    tree :: CondTree ConfVar [Dependency] (Set Binding) -> [Doc]
    tree (CondNode d _ branches) = node $ fmap branch branches
      where
        binding = text . view (localName.ident)
        node =
          if null d
          then id
          else (:) $ brackets $ sep $ binding <$> Set.toList d


    lists :: Bool -> [Doc] -> Doc
    lists root = \case
      [] -> mempty
      [x] -> x
      xs -> (if root then id else parens) $
        indented (text "builtins.concatLists" <+> lbrack) rbrack $
          fsep xs

    branch :: CondBranch ConfVar [Dependency] (Set Binding) -> Doc
    branch (CondBranch c t mf) = parens $ case mf of
      Nothing -> sep
        [ text "lib.optionals",
          nest 2 $ condition c,
          nest 2 $ parens $ go t
        ]
      Just f -> sep
        [ text "if"   <+> condition c
        , text "then" <+> go t
        , text "else" <+> go f
        ]
      where go = lists False . tree

    condition :: Condition ConfVar -> Doc
    condition = fix $ \go -> \case
      Var v -> var v
      Lit b -> bool b
      CNot a -> text "!" <> go a
      COr a b -> parens $ go a <> text " || " <> go b
      CAnd a b -> parens $ go a <> text " && " <> go b

    var = \case
      Arch x -> text $ "pkgs.stdenv.is" ++ arch x
      Impl flavor range -> impl flavor range
      OS x -> text $ "pkgs.stdenv.is" ++ os x
      PackageFlag f -> text $ "flag:" ++ unFlagName f --TODO

    arch = \case
      AArch64 -> "Aarch64"
      JavaScript -> "Ghcjs"
      X86_64 -> "x86_64"
--      Wasm32 -> "Wasm"
      x -> error $ "TODO: aarch: " ++ show x

    --TODO: take ranges into account
    impl flavor range = case flavor of
      GHC -> text "!pkgs.stdenv.isGhcjs"
      GHCJS -> text "pkgs.stdenv.isGhcjs"
      x -> error $ "TODO: impl: " ++ show x

    os = \case
      Android -> "Android"
--      Ghcjs -> "Ghcjs"
      IOS -> "iOS"
      Linux -> "Linux"
      OSX -> "MacOS"
      Wasi -> "Wasi"
      Windows -> "Windows"
      x -> error $ "TODO: os: " ++ show x
