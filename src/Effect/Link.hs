module Effect.Link where

import Control.Monad.Except (ExceptT)
import Control.Exception (IOException, try)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(..))
import System.Path (DirPath, takeDirectory)
import System.Process.Typed (runProcess, proc)
import Control.Monad.IO.Class (liftIO)
import Control.Error (hoistEither)
import qualified Data.Path as Path

createDirectory path = do
  r <- liftIO $ try $ createDirectoryIfMissing True $ Path.toString path
  hoistEither r

linkStaticLibrary :: [Path.ObjectFile] -> Path.StaticLibraryFile -> ExceptT IOException IO Bool
linkStaticLibrary objects (Path.StaticLibraryFile lib) = do
  let objects' = (\(Path.ObjectFile obj) -> obj) <$> objects
  _ <- createDirectory $ takeDirectory lib

  let cmd = "libtool"
  let args = [ "--mode=link", "g++"] <> (Path.toString <$> objects') <> ["-o", Path.toString lib]

  liftIO $ putStrLn $ show $ [cmd] <> args
  exitCode <- (liftIO $ try $ runProcess $ proc cmd args) >>= hoistEither

  case exitCode of
    ExitFailure x ->
      return False
    ExitSuccess ->
      return True

linkExecutable :: [Path.ObjectFile] -> Path.ExecutableFile -> ExceptT IOException IO Bool
linkExecutable objects (Path.ExecutableFile executable) = do
  let objects' = (\(Path.ObjectFile obj) -> obj) <$> objects
  _ <- createDirectory $ takeDirectory executable

  let cmd = "libtool"
  let args = [ "--mode=link", "g++"] <> (Path.toString <$> objects') <> ["-o", Path.toString executable]

  liftIO $ putStrLn $ show $ [cmd] <> args
  exitCode <- (liftIO $ try $ runProcess $ proc cmd args) >>= hoistEither

  case exitCode of
    ExitFailure x ->
      return False
    ExitSuccess ->
      return True
