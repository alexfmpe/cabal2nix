{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Distribution.Nixpkgs.Haskell.BuildInfo
  ( BuildInfo
  , haskell, pkgconfig, system, tool, pPrintBuildInfo, condTreeData
  )
  where

import Control.DeepSeq
import Control.Lens
import Data.Semigroup as Sem
import Data.Set ( Set )
import Distribution.PackageDescription (CondTree, ConfVar, Dependency, FlagAssignment)
import qualified Distribution.PackageDescription as PackageDescription
import GHC.Generics ( Generic )
import Language.Nix
import Language.Nix.PrettyPrinting hiding ( (<>) )

data BuildInfo = BuildInfo
  { _haskell   :: Set Binding
  , _pkgconfig :: Set Binding
  , _system    :: Set Binding
  , _tool      :: Set Binding
  }
  deriving (Show, Eq, Generic)

makeLenses ''BuildInfo

instance Each BuildInfo BuildInfo (Set Binding) (Set Binding) where
  each f (BuildInfo a b c d) = BuildInfo <$> f a <*> f b <*> f c <*> f d

instance Sem.Semigroup BuildInfo where
  BuildInfo w1 x1 y1 z1 <> BuildInfo w2 x2 y2 z2 =
    BuildInfo (w1 <> w2) (x1 <> x2) (y1 <> y2) (z1 <> z2)

instance Monoid BuildInfo where
  mempty = BuildInfo mempty mempty mempty mempty
  mappend = (Sem.<>)

instance NFData BuildInfo

pPrintBuildInfo :: String -> FlagAssignment -> CondTree ConfVar [Dependency] BuildInfo -> Doc
pPrintBuildInfo prefix cabalFlags tree = vcat
  [ f _haskell   "HaskellDepends"
  , f _system    "SystemDepends"
  , f _pkgconfig "PkgconfigDepends"
  , f _tool      "ToolDepends"
  ]
  where
    f field suffix = condTreeAttr (prefix++suffix) cabalFlags $ fmap field tree


condTreeData :: Lens' (CondTree v c a) a
condTreeData = lens PackageDescription.condTreeData $ \c a -> c { PackageDescription.condTreeData = a }
