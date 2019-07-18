module Hash (hashFile, toHex) where

import Prelude

import Crypto.Hash.SHA1 (hashlazy)
import Text.Printf (printf)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import Control.Exception (IOException, try)

hashFile :: FilePath -> IO (Either IOException Strict.ByteString)
hashFile = try . fmap hashlazy . Lazy.readFile

toHex :: Strict.ByteString -> String
toHex bytes = Strict.unpack bytes >>= printf "%02x"
