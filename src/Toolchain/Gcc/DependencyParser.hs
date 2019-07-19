module Toolchain.Gcc.DependencyParser where

import Prelude

import Data.Coerce (coerce)
import Data.Either (Either(..))
import Data.Either.Combinators (mapLeft)
import Data.Makefile
import Data.Makefile.Parse
import Data.Path
import Data.Text (pack, unpack)
import System.Path (relFile)
import qualified Data.Path as Path

dependencies' :: Entry -> [Dependency]
dependencies' (Rule _ dependencies _) = dependencies
dependencies' _ = []

dependencies :: [Entry] -> [Dependency]
dependencies e = e >>= dependencies'

unwrap :: Dependency -> Path.HeaderFile
unwrap (Dependency d) = Path.HeaderFile $ relFile . unpack $ d

parse :: String -> Either String [Path.HeaderFile]
parse contents = makefile >>= return . drop 1 . map unwrap . dependencies . entries
  where makefile = parseMakefileContents $ pack contents
