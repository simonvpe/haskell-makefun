module Effect.Compile where

import Prelude

import Control.Error (hoistEither)
import Control.Exception (IOException, try)
import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Hash as Hash
import Data.Path as Path
import Effect.CompileSpec
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(..))
import System.Path (DirPath, takeDirectory)
import System.Process.Typed (runProcess, proc)
import Control.Concurrent
import System.Random

createDirectory path = do
  r <- liftIO $ try $ createDirectoryIfMissing True $ Path.toString path
  hoistEither r

compile :: (String -> Either String [Path.HeaderFile]) -> CompileSpec -> ExceptT IOException IO Bool
compile parse spec = do
  let (Path.SourceFile src, Path.ObjectFile obj, Path.DependFile dep) = (sourceFile spec, objectFile spec, dependFile spec)

  _ <- createDirectory $ takeDirectory obj
  _ <- createDirectory $ takeDirectory dep

  let cmd = "g++"
  let args = [ "-o", Path.toString obj, "-c", "-MMD", Path.toString src ]

  liftIO $ putStrLn $ show $ [cmd] <> args
  exitCode <- (liftIO $ try $ runProcess $ proc cmd args) >>= hoistEither
  _ <- runProcess $ proc "sync" []
  --us <- liftIO $ randomRIO (1000000, 10000000 :: Int)
  --_ <- liftIO $ threadDelay us

  case exitCode of
    ExitFailure x ->
      return False
    ExitSuccess -> do
      dependHash <- liftIO $ readDependHash parse $ dependFile spec
      case Hash.calculateChecksum <$> (Just $ sourceHash spec) <*> dependHash of
        Just checksum -> do
          writeChecksum (checksumFile spec) checksum
          return True
        Nothing ->
          return True
