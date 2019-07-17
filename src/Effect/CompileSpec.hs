module Effect.CompileSpec
  ( CompileSpec(sourceFile, objectFile, dependFile, checksumFile, sourceHash)
  , make
  , readDependHash
  , writeChecksum
  ) where

import Prelude

import Control.Error (hoistEither)
import Control.Exception (IOException, try)
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Internal (unpackChars)
import Data.CompileSpec
import Data.Either.Combinators (rightToMaybe)
import System.Directory (doesFileExist)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Path as Path
import qualified Effect.Hash as Hash

readDependHash :: Path.DependFile -> IO (Maybe Hash.DependHash)
readDependHash (Path.DependFile dependPath) = do
  dependHash <- Hash.hashFile (Path.toString dependPath)
  return $ rightToMaybe dependHash >>= return . Hash.DependHash .  Hash.toHex

readSourceHash :: Path.SourceFile -> ExceptT IOException IO Hash.SourceHash
readSourceHash (Path.SourceFile sourcePath) = do
  sourceHash' <- liftIO $ Hash.hashFile (Path.toString sourcePath)
  hoistEither $ sourceHash' >>= return . Hash.SourceHash . Hash.toHex

readChecksum :: Path.ChecksumFile -> IO (Maybe Hash.ObjectChecksum)
readChecksum (Path.ChecksumFile checksumPath) = do
  checksum <- try (Strict.readFile $ Path.toString checksumPath):: IO (Either IOException Strict.ByteString)
  pure $ rightToMaybe checksum >>= pure . Hash.ObjectChecksum . unpackChars

writeChecksum :: Path.ChecksumFile -> Hash.ObjectChecksum -> ExceptT IOException IO ()
writeChecksum (Path.ChecksumFile path) (Hash.ObjectChecksum checksum) = do
  r <- liftIO $ try $ writeFile (Path.toString path) checksum
  hoistEither $ r
  

make :: Path.BuildDir -> Path.SourceFile -> ExceptT IOException IO (Maybe CompileSpec)
make buildDir (Path.SourceFile src) = do
  sourceHash <- readSourceHash (Path.SourceFile src)
  let dependFile = Path.srcToDep buildDir (Path.SourceFile src) sourceHash
  dependHash <- liftIO $ readDependHash dependFile
  let checksumFile = Path.srcToChecksum buildDir (Path.SourceFile src) sourceHash
  storedChecksum <- liftIO $ readChecksum checksumFile
  let calculatedChecksum = Hash.calculateChecksum <$> (Just sourceHash) <*> dependHash
  let (Path.ObjectFile objectFile) = Path.srcToObj buildDir (Path.SourceFile src) sourceHash
  let checksumMatches = (==) <$> calculatedChecksum <*> storedChecksum
  objectExists <- liftIO $ doesFileExist $ Path.toString objectFile
  pure $ case (&&) <$> (Just objectExists) <*> checksumMatches  of
    Just True -> Nothing
    _         -> Just $ CompileSpec { sourceFile   = (Path.SourceFile src)
                                    , objectFile   = (Path.ObjectFile objectFile)
                                    , dependFile   = dependFile
                                    , checksumFile = checksumFile
                                    , sourceHash   = sourceHash
                                    }
