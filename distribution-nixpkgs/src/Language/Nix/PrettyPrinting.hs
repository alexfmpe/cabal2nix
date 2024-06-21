{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GADTs #-}

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
import Control.Lens ( view )
import Data.Bifunctor (bimap)
import Data.Char
import Data.Function
import Data.List (sortBy)
import Data.Set ( Set )
import qualified Data.Set as Set
import Distribution.Compiler (CompilerFlavor(GHC))
import Distribution.PackageDescription (CondBranch(..), Condition (..), CondTree(CondNode), ConfVar (..), Dependency, FlagAssignment, lookupFlagAssignment)
import Distribution.System (OS(..), Arch (..))
import Language.Nix (ident)
import Language.Nix.Binding (Binding, localName)
import "pretty" Text.PrettyPrint.HughesPJClass
import Data.Maybe (isJust)
import Data.Semigroup (Arg(..))

attr :: String -> Doc -> Doc
attr n v = text n <+> equals <+> v <> semi

onlyIf :: Bool -> Doc -> Doc
onlyIf b d = if b then d else empty

boolattr :: String -> Bool -> Bool -> Doc
boolattr n p v = if p then attr n (bool v) else empty
  where
    bool :: Bool -> Doc
    bool True  = text "true"
    bool False = text "false"

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

data Deps where
  Attribute :: String -> Deps
  List :: [Deps] -> Deps
  ConcatLists :: [Deps] -> Deps
  Optionals :: Doc -> Deps -> Deps
  IfThenElse :: Doc -> Deps -> Deps -> Deps
  deriving Eq

ppDeps :: Deps -> Doc
ppDeps = \case
  Attribute x -> text x
  List xs -> sep
    [ lbrack
    , nest 2 $ fsep $ fmap go xs
    , rbrack
    ]
  ConcatLists xs -> sep
    [ text "builtins.concatLists" <+> lbrack
    , nest 2 $ fsep $ fmap go xs
    , rbrack
    ]
  Optionals c t -> sep
    [ text "lib.optionals"
    , c
    , go t
    ]
  IfThenElse c t f -> sep
    [ text "if"   <+> c
    , text "then" <+> go t
    , text "else" <+> go f
    ]
  where
    go x = ppDeps x & case x of
      Attribute {} -> id
      ConcatLists {} -> parens
      IfThenElse {} -> parens
      List {} -> id
      Optionals {} -> parens

condTreeAttr :: String -> FlagAssignment -> CondTree ConfVar [Dependency] (Set Binding) -> Doc
condTreeAttr n cabalFlags tr = case tree tr of
  List [] -> mempty
  List xs -> list mempty xs
  ConcatLists xs -> list (text "builtins.concatLists") xs
  t -> sep
    [ text n <+> equals,
      nest 2 $ ppDeps t,
      semi
    ]
  where
    list prefix = listattrDoc n prefix . fmap ppDeps

    tree :: CondTree ConfVar [Dependency] (Set Binding) -> Deps
    tree (CondNode d _ branches) = case (nodes, canon branches) of
      ([], []) -> List []
      (xs, []) -> List xs
      ([], [y]) -> y
      ([], ys) -> lists ys
      (xs, ys) -> lists $ List xs : ys
      where
        name = view (localName . ident)
        nodes = node <$> toAscListSortedOn name d
        node = Attribute . name
        canon = filter (/= List []) . map branch

        getAttr dep = case dep of
          Attribute doc -> Just $ Arg doc dep
          _ -> Nothing

        exclusivelyAttrs = \case
          List xs -> traverse getAttr xs
          _ -> Nothing

        lists xs = case traverse exclusivelyAttrs xs of
          Nothing -> ConcatLists xs
          Just ys -> List $ fmap argVal $ toAscListSortedOn argKey $ Set.fromList $ concat ys
            where
              argKey (Arg k _) = k
              argVal (Arg _ v) = v

    branch :: CondBranch ConfVar [Dependency] (Set Binding) -> Deps
    branch (CondBranch c true mfalse) = case (condition c, tree true, maybe (List []) tree mfalse) of
      (_, List [], List []) -> List []
      (Left True,  t, _) -> t
      (Left False, _, f) -> f
      (Right d, List [], f) -> Optionals (text "!" <> d) f
      (Right d, t, List []) -> Optionals d t
      (Right d, t, f) -> IfThenElse d t f

    condition :: Condition ConfVar -> Either Bool Doc
    condition = \case
      Var v -> var v
      Lit a -> Left a
      CNot a -> unary "!" not a
      COr a b -> binary "||" (||) False a b
      CAnd a b -> binary "&&" (&&) True a b
      where
        unary nix hask a = bimap hask (\d -> parens $ text nix <> d) (condition a)
        binary nix hask identity a b = case (condition a, condition b) of
          (Left l,  Left r) -> Left (l `hask` r)
          (Right l, Right r) -> Right $ parens $ l <> text (" " ++ nix ++ " ") <> r
          (Left l,  Right r) -> case l == identity of
            False -> Left l
            True -> Right r
          (Right l, Left r) -> case r == identity of
            False -> Left r
            True -> Right l

    var :: ConfVar -> Either Bool Doc
    var = \case
      Arch x -> case x of
        AArch64 -> is "Aarch64"
        JavaScript -> is "Ghcjs"
        PPC64 -> is "Power64"
        X86_64 -> is "x86_64"
        Wasm32 -> is "Wasm"
        _ -> unknown
      Impl flavor _ -> Left $ flavor == GHC
      OS x -> case x of
        Android -> is "Android"
        FreeBSD -> is "FreeBSD"
        Ghcjs -> is "Ghcjs"
        IOS -> is "iOS"
        Linux -> is "Linux"
        NetBSD -> is "NetBSD"
        OpenBSD -> is "OpenBSD"
        OSX -> is "MacOS"
        Wasi -> is "Wasi"
        Windows -> is "Windows"
        _ -> unknown
      PackageFlag name -> Left $ isJust $ lookupFlagAssignment name cabalFlags

      where
        is :: String -> Either Bool Doc
        is x = Right $ text $ "pkgs.stdenv.hostPlatform.is" ++ x

        unknown :: Either Bool Doc
        unknown = Left False
