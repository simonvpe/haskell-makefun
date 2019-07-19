module Data.Path
  ( BuildDir(..)
  , SourceFile(..)
  , ObjectFile(..)
  , DependFile(..)
  , ChecksumFile(..)
  , StaticLibraryFile(..)
  , srcToObj
  , srcToDep
  , srcToChecksum
  , module System.Path
  ) where

import Prelude

import System.Path (RelFile, RelDir, relDir, relFile, toString, takeDirectory, combine, dirFromFile, (</>), (<.>))
import Data.Hash (SourceHash(..))

newtype BuildDir = BuildDir RelDir deriving (Show, Eq)
newtype SourceFile = SourceFile RelFile deriving (Show, Eq)
newtype ObjectFile = ObjectFile RelFile deriving (Show, Eq)
newtype DependFile = DependFile RelFile deriving (Show, Eq)
newtype ChecksumFile = ChecksumFile RelFile deriving (Show, Eq)
newtype StaticLibraryFile = StaticLibraryFile RelFile deriving (Show, Eq)

srcToObj :: BuildDir -> SourceFile -> SourceHash -> ObjectFile
srcToObj (BuildDir buildDir) (SourceFile src) (SourceHash hash) =
  ObjectFile $ combine buildDir $ dirFromFile src </> relFile hash <.> ".o"

srcToDep :: BuildDir -> SourceFile -> SourceHash -> DependFile
srcToDep (BuildDir buildDir) (SourceFile src) (SourceHash hash) =
  DependFile $ combine buildDir $ dirFromFile src </> relFile hash <.> ".d"

srcToChecksum :: BuildDir -> SourceFile -> SourceHash -> ChecksumFile
srcToChecksum (BuildDir buildDir) (SourceFile src) (SourceHash hash) =
  ChecksumFile $ combine buildDir $ dirFromFile src </> relFile hash <.> ".k"
