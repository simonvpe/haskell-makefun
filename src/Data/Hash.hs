module Data.Hash
  ( SourceHash(..)
  , DependHash(..)
  , ObjectChecksum(..)
  , calculateChecksum
  , toHex
  ) where

import Prelude

import Control.Exception (IOException, try)
import Text.Printf (printf)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy

newtype SourceHash = SourceHash String deriving (Show)
newtype DependHash = DependHash String deriving (Show)
newtype ObjectChecksum = ObjectChecksum String deriving(Show, Eq)

toHex :: Strict.ByteString -> String
toHex bytes = Strict.unpack bytes >>= printf "%02x"

calculateChecksum :: SourceHash -> DependHash -> ObjectChecksum
calculateChecksum (SourceHash s) (DependHash d) = ObjectChecksum $ s <> d
