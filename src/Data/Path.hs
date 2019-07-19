module Data.Path
  ( BuildDir(..)
  , SourceFile(..)
  , ObjectFile(..)
  , DependFile(..)
  , ChecksumFile(..)
  , StaticLibraryFile(..)
  , HeaderFile(..)
  , ExecutableFile(..)
  , srcToObj
  , srcToDep
  , srcToChecksum
  , module System.Path
  ) where

import Prelude

import System.Path (RelFile, RelDir, relDir, relFile, toString, takeDirectory, combine, dirFromFile, (</>), (<.>))
import Data.Hash (SourceHash(..))

newtype BuildDir = BuildDir RelDir deriving (Show, Eq, Ord)
newtype SourceFile = SourceFile RelFile deriving (Show, Eq, Ord)
newtype ObjectFile = ObjectFile RelFile deriving (Show, Eq, Ord)
newtype DependFile = DependFile RelFile deriving (Show, Eq, Ord)
newtype HeaderFile = HeaderFile RelFile deriving (Show, Eq, Ord)
newtype ChecksumFile = ChecksumFile RelFile deriving (Show, Eq, Ord)
newtype StaticLibraryFile = StaticLibraryFile RelFile deriving (Show, Eq, Ord)
newtype ExecutableFile = ExecutableFile RelFile deriving (Show, Eq, Ord)

srcToObj :: BuildDir -> SourceFile -> SourceHash -> ObjectFile
srcToObj (BuildDir buildDir) (SourceFile src) (SourceHash hash) =
  ObjectFile $ combine buildDir $ dirFromFile src </> relFile hash <.> ".o"

srcToDep :: BuildDir -> SourceFile -> SourceHash -> DependFile
srcToDep (BuildDir buildDir) (SourceFile src) (SourceHash hash) =
  DependFile $ combine buildDir $ dirFromFile src </> relFile hash <.> ".d"

srcToChecksum :: BuildDir -> SourceFile -> SourceHash -> ChecksumFile
srcToChecksum (BuildDir buildDir) (SourceFile src) (SourceHash hash) =
  ChecksumFile $ combine buildDir $ dirFromFile src </> relFile hash <.> ".k"
