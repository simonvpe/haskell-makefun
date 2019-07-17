module Data.CompileSpec (CompileSpec(CompileSpec, sourceFile, objectFile, dependFile, checksumFile, sourceHash)) where

import qualified Data.Hash as Hash
import qualified Data.Path as Path

data CompileSpec =
  CompileSpec { sourceFile :: Path.SourceFile
              , objectFile :: Path.ObjectFile
              , dependFile :: Path.DependFile
              , checksumFile :: Path.ChecksumFile
              , sourceHash :: Hash.SourceHash
              } deriving (Show)
