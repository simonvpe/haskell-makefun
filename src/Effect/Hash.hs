module Effect.Hash
  ( SourceHash(SourceHash)
  , DependHash(DependHash)
  , ObjectChecksum(ObjectChecksum)
  , toHex
  , calculateChecksum
  , hashFile
  ) where

import Prelude

import Control.Exception (IOException, try)
import Control.Monad.Except (ExceptT)
import Crypto.Hash.SHA1 (hashlazy)
import Data.Hash
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy

hashFile :: FilePath -> IO (Either IOException Strict.ByteString)
hashFile = try . fmap hashlazy . Lazy.readFile
