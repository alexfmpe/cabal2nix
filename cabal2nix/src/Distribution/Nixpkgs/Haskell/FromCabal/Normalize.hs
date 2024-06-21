{-# LANGUAGE OverloadedStrings #-}

module Distribution.Nixpkgs.Haskell.FromCabal.Normalize ( normalize ) where

import Control.Lens
import qualified Data.Set as Set
import Data.String
import Distribution.Nixpkgs.Haskell
import Distribution.Nixpkgs.Meta
import Distribution.Package
import Language.Nix hiding ( quote )

normalize :: Derivation -> Derivation
normalize drv = drv
  & bi libraryDepends
  & bi executableDepends
  & bi testDepends
  & bi benchmarkDepends
  & over metaSection normalizeMeta
  & jailbreak %~ (&& (packageName drv /= "jailbreak-cabal"))
  where bi l = over (l . condTreeData) (normalizeBuildInfo (packageName drv))

normalizeBuildInfo :: PackageName -> BuildInfo -> BuildInfo
normalizeBuildInfo pname bi = bi
  & haskell %~ Set.filter (\b -> view localName b /= fromString (unPackageName pname))
  & tool %~ Set.filter (\b -> view localName b /= fromString (unPackageName pname))

normalizeMeta :: Meta -> Meta
normalizeMeta meta = meta
  & description %~ normalizeSynopsis
  & hydraPlatforms %~ (if meta^.broken then const (Just Set.empty) else id)

normalizeSynopsis :: String -> String
normalizeSynopsis desc
  | null desc                                             = []
  | last desc == '.' && length (filter ('.'==) desc) == 1 = normalizeSynopsis (init desc)
  | otherwise                                             = quote (unwords (words desc))

quote :: String -> String
quote ('\\':c:cs) = '\\' : c : quote cs
quote ('"':cs)    = '\\' : '"' : quote cs
quote (c:cs)      = c : quote cs
quote []          = []
