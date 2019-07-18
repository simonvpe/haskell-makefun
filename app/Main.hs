{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import ElmArchitecture (Config(_init, _update, Config), Cmd)
import Prelude hiding (init, log)
import System.Path (RelFile, RelDir, relDir, relFile, toString, takeDirectory, combine, dirFromFile, (</>), (<.>))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified ElmArchitecture
import qualified Hash
import Control.Monad.Except (ExceptT)
import Control.Exception (IOException)
import Control.Error (hoistEither)
import Control.Monad.IO.Class (liftIO)
import Data.Either.Combinators (rightToMaybe)
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import Control.Exception (IOException, try)
import Text.Printf (printf)
import Data.Char (chr)
import Data.ByteString.Internal (unpackChars)

data Model =
  Model { nofTasks :: Int
        , maxNofTasks :: Int
        , buildDir :: RelDir
        , sourceHash :: Map RelFile String
        } deriving (Show)

data LogLevel
  = Debug
  | Info
  | Warning
  | Error
  deriving (Show)

newtype SourceFile = SourceFile RelFile deriving (Show)
newtype ObjectFile = ObjectFile RelFile deriving (Show)
newtype DependFile = DependFile RelFile deriving (Show)
newtype ChecksumFile = ChecksumFile RelFile deriving (Show)
newtype SourceHash = SourceHash String deriving (Show)
newtype DependHash = DependHash String deriving (Show)

data Msg
  = Noop
  | Log LogLevel String
  | Compile SourceFile ObjectFile SourceHash
  | FatalError String
  deriving (Show)

srcToObj :: Model -> SourceFile -> SourceHash -> ObjectFile
srcToObj model (SourceFile src) (SourceHash hash) =
  ObjectFile $ combine (buildDir model) $ dirFromFile src </> relFile hash <.> ".o"

srcToDep :: Model -> SourceFile -> SourceHash -> DependFile
srcToDep model (SourceFile src) (SourceHash hash) =
  DependFile $ combine (buildDir model) $ dirFromFile src </> relFile hash <.> ".d"

srcToChecksum :: Model -> SourceFile -> SourceHash -> ChecksumFile
srcToChecksum model (SourceFile src) (SourceHash hash) =
  ChecksumFile $ combine (buildDir model) $ dirFromFile src </> relFile hash <.> ".k"

dependHash :: DependFile -> IO (Maybe String)
dependHash (DependFile dependPath) = do
  dependHash' <- Hash.hashFile (toString dependPath)
  return $ rightToMaybe dependHash' >>= return . Hash.toHex

readChecksum :: ChecksumFile -> IO (Maybe String)
readChecksum (ChecksumFile checksumPath) = do
  checksum <- try (Strict.readFile $ toString checksumPath):: IO (Either IOException Strict.ByteString)
  pure $ rightToMaybe checksum >>= pure . unpackChars

initiateCompilation' :: Model -> SourceFile -> ExceptT IOException IO Msg
initiateCompilation' model (SourceFile src) = do
  sourceHash' <- liftIO $ Hash.hashFile (toString src)
  sourceHash <- hoistEither $ sourceHash' >>= return . Hash.toHex

  let dependFile = srcToDep model (SourceFile src) (SourceHash sourceHash)
  let checksumFile = srcToChecksum model (SourceFile src) (SourceHash sourceHash)

  r <- liftIO $ (<>) <$> dependHash dependFile <*> readChecksum checksumFile
  case r of
    Just h -> liftIO $ putStrLn ("Found hash " <> h)
    Nothing -> liftIO $ putStrLn "Couldn't find hash"
  --dependHash <- hoistEither $ 
  pure Noop

initiateCompilation :: Model -> SourceFile -> IO Msg
initiateCompilation model (SourceFile src) = do
  sourceHash' <- Hash.hashFile (toString src)
  case sourceHash' of
    Left err ->
      return $ FatalError $ show err
    Right contents -> do
      let sourceFile = SourceFile src
      let sourceHash = SourceHash $ Hash.toHex contents

      let (DependFile dependFile) = srcToDep model (SourceFile src) sourceHash
      dependHash' <- Hash.hashFile (toString dependFile)

      let objectFile = srcToObj model (SourceFile src) sourceHash

      return $ Compile sourceFile objectFile sourceHash

needsRecompile :: Model -> SourceFile -> ObjectFile -> SourceHash -> IO Msg
needsRecompile model (SourceFile src) (ObjectFile object) (SourceHash sourceHash) = do
  --dependPath = srcToDep :: Model -> SourceFile -> 
  --depend <- Hash.hashFile ()
  pure Noop

update :: Msg -> Model -> (Model, Cmd Msg)
update msg model = do
  case msg of
    Noop -> (model, [])
    Compile source object sourceHash -> (model, [])
    FatalError msg -> (model, [])



log :: LogLevel -> Msg -> IO (Msg)
log level msg = do
  putStrLn $ show msg
  return Noop

updateWithLog :: (Msg -> Model -> (Model, Cmd Msg)) -> Msg -> Model -> (Model, Cmd Msg)
updateWithLog update msg model = case msg of
  Noop -> update msg model
  _ -> do
    let (model', cmd') = update msg model
    (model', [log Debug $ msg] <> cmd')

initialModel :: Model
initialModel =
  Model { nofTasks = 0
        , maxNofTasks = 8
        , buildDir = relDir "build"
        , sourceHash = Map.empty
        }

initialCmds :: Cmd Msg
initialCmds =
  [initiateCompilation initialModel (SourceFile $ relDir "cpp" </> relFile "test" <.> "cpp")]

main :: IO ()
main = ElmArchitecture.run
  Config { _init = (initialModel, initialCmds)
         , _update = updateWithLog update }
