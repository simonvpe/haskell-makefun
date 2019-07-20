{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Lens hiding ((:>), at, (<.>), _init)
import Data.Foldable (fold)
import Data.Map.Strict (Map)
import Data.Path ((</>), (<.>))
import Data.List (delete, (\\))
import ElmArchitecture (Config(_init, _update, Config), Cmd)
import Prelude hiding (init, log)
import Control.Concurrent (threadDelay)
import qualified Data.Hash as Hash
import qualified Data.Map.Strict as Map
import qualified Data.Path as Path
import qualified Effect.Compile as Compile
import qualified Effect.Link as Link
import qualified Effect.CompileSpec as CompileSpec
import qualified ElmArchitecture
import qualified Toolchain.Gcc.DependencyParser as DependencyParser
import Debug.Trace

-- |
-- | MODEL
-- |

data Task
  = Compile Path.SourceFile
  | LinkExecutable [Path.SourceFile] Path.ExecutableFile
  | LinkStaticLibrary [Path.SourceFile] Path.StaticLibraryFile
  deriving (Show, Eq)

newtype TaskId = TaskId Int deriving (Show, Eq)

data Job = Job { _taskId :: TaskId, _task :: Task, _depends :: [TaskId] } deriving (Show, Eq)

data Model =
  Model { _maxNofTasks :: Int
        , _buildDir :: Path.BuildDir
        , _pending :: [Job]
        , _inProgress :: [Job]
        , _completed :: [TaskId]
        , _objects :: Map Path.SourceFile Path.ObjectFile
        } deriving (Show)

makeLenses ''Job
makeLenses ''Model

-- |
-- | EVENTS
-- |

data LogLevel
  = Debug
  | Info
  | Warning
  | Error
  deriving (Show)

data Msg
  = Noop
  | Keepalive
  | AddTask Job
  | FinalizeTask Job Msg
  | FatalError String
  | AddObject Path.SourceFile Path.ObjectFile
  | Many [Msg]
  deriving (Show)

compile :: Job -> Path.BuildDir -> Path.SourceFile -> IO Msg
compile job buildDir sourceFile = do
  spec <- runExceptT $ CompileSpec.make DependencyParser.parse buildDir sourceFile
  case spec of
    Left err -> return $ FatalError $ show err
    Right mspec -> do
      let cmd = AddObject (CompileSpec.sourceFile mspec) (CompileSpec.objectFile mspec)
      if CompileSpec.rebuildRequired mspec
        then do
          result <- runExceptT $ Compile.compile DependencyParser.parse mspec
          case result of
            Left err    -> return $ FatalError $ show err
            Right False -> return $ FatalError "compilation failed"
            Right True  -> return $ FinalizeTask job $ cmd
        else
          return $ FinalizeTask job $ cmd

linkStaticLibrary :: Job -> Path.BuildDir -> [Path.ObjectFile] -> Path.StaticLibraryFile -> IO Msg
linkStaticLibrary job buildDir objectPaths library = do
  putStrLn $ show objectPaths
  result <- runExceptT $ Link.linkStaticLibrary objectPaths library
  case result of
    Left err -> return $ FatalError $ show err
    Right False -> return $ FatalError "linking failed"
    Right True -> return $ FinalizeTask job Noop

linkExecutable :: Job -> Path.BuildDir -> [Path.ObjectFile] -> Path.ExecutableFile -> IO Msg
linkExecutable job buildDir objectPaths executable = do
  putStrLn $ show objectPaths
  result <- runExceptT $ Link.linkExecutable objectPaths executable
  case result of
    Left err -> return $ FatalError $ show err
    Right False -> return $ FatalError "linking failed"
    Right True -> return $ FinalizeTask job Noop

runJob :: Job -> Model -> IO Msg
runJob job model =
  case job^.task of
    Compile sourceFile ->
      compile job (model^.buildDir) sourceFile

    LinkStaticLibrary sources library -> do
      case sequence $ flip Map.lookup (model^.objects) <$> sources of
        Just objectPaths -> linkStaticLibrary job (model^.buildDir) objectPaths library
        Nothing -> pure $ FatalError "missing object file"

    LinkExecutable sources executable -> do
      case sequence $ flip Map.lookup (model^.objects) <$> sources of
        Just objectPaths -> linkExecutable job (model^.buildDir) objectPaths executable
        Nothing -> pure $ FatalError "missing object file"

shutDown :: IO Msg
shutDown = do
  putStrLn "Shutting down!"
  pure $ Noop

-- keepalive :: IO Msg
-- kkepalize = do
--   _ <- threadDelay 10000.0
--   pure $ Keepalive

update :: Msg -> Model -> (Model, Cmd Msg)
update msg model = do
  case msg of
    Noop ->
      (model, [])

    -- Keepalive -> if model^.inProgress
    --   (model, [keepalive])

    AddTask job ->
      if (model^.inProgress^.to length) < (model^.maxNofTasks) && (job^.depends^.to length) == 0 then
        (inProgress %~ ((++) [job]) $ model, [runJob job model])
      else
        (pending %~ ((++) [job]) $ model, [])

    FinalizeTask job msg -> do
      let (model', cmds) = update msg model -- immediate fold of this message
          pendingRevised = (depends %~ (delete $ job^.taskId)) <$> model'^.pending
          n = (model'^.maxNofTasks) - (model'^.inProgress.to length) + 1
          ready = filter (\x -> x^.depends.to length == 0) pendingRevised
          notReady = pendingRevised \\ ready
          model'' = pending .~ (drop n ready <> notReady)
                   $ inProgress %~ (delete job)
                   $ completed %~ ((++) [job^.taskId])
                   $ model'
          cmds' = flip runJob model'' <$> take n ready
      (model'', cmds <> cmds')

    AddObject src obj -> do
      (objects %~ (Map.insert src obj) $ model, [])

    Many msgs ->
      (model, return <$> msgs)

    FatalError msg ->
      (pending .~ [] $ inProgress .~ [] $ model, [shutDown])




addCompileTask :: TaskId -> [TaskId] -> Path.SourceFile -> IO Msg
addCompileTask _taskId _depends sourceFile = pure $ AddTask $ Job {_taskId, _depends, _task = Compile sourceFile}

addLinkExecutableTask :: TaskId -> [TaskId] -> [Path.SourceFile] -> Path.ExecutableFile -> IO Msg
addLinkExecutableTask _taskId _depends sources executable =
  pure $ AddTask $ Job { _taskId
                       , _depends
                       , _task = LinkExecutable sources executable}

addLinkStaticLibraryTask :: TaskId -> [TaskId] -> [Path.SourceFile] -> Path.StaticLibraryFile -> IO Msg
addLinkStaticLibraryTask _taskId _depends sources output =
  pure $ AddTask $ Job { _taskId
                       , _depends
                       , _task = LinkStaticLibrary sources output
                       }

test1 = Path.SourceFile $ Path.relDir "cpp" </> Path.relFile "test" <.> "cpp"
test2 = Path.SourceFile $ Path.relDir "cpp" </> Path.relFile "test2" <.> "cpp"
test3 = Path.SourceFile $ Path.relDir "cpp" </> Path.relFile "test3" <.> "cpp"
test4 = Path.SourceFile $ Path.relDir "cpp" </> Path.relFile "test4" <.> "cpp"
test5 = Path.SourceFile $ Path.relDir "cpp" </> Path.relFile "test5" <.> "cpp"
test6 = Path.SourceFile $ Path.relDir "cpp" </> Path.relFile "test6" <.> "cpp"
test7 = Path.SourceFile $ Path.relDir "cpp" </> Path.relFile "test7" <.> "cpp"
test8 = Path.SourceFile $ Path.relDir "cpp" </> Path.relFile "test8" <.> "cpp"
lib = Path.StaticLibraryFile $ Path.relDir "build" </> Path.relFile "lib" <.> "a"
exe = Path.ExecutableFile $ Path.relDir "build" </> Path.relFile "exe"

initialCmds :: Cmd Msg
initialCmds =
  [ addCompileTask (TaskId 1) [] test1
  , addCompileTask (TaskId 2) [] test2
  , addCompileTask (TaskId 3) [] test3
  , addCompileTask (TaskId 4) [] test4
  , addLinkExecutableTask (TaskId 5) ([TaskId 10] <> (TaskId <$> [1, 2, 3, 4])) [test1, test2, test3, test4] exe
  , addCompileTask (TaskId 6) [] test5
  , addCompileTask (TaskId 7) [] test6
  , addCompileTask (TaskId 8) [] test7
  , addCompileTask (TaskId 9) [] test8
  , addLinkStaticLibraryTask (TaskId 10) (TaskId <$> [6, 7, 8, 9]) [test5, test6, test7, test8] lib
  ]

















log :: LogLevel -> Msg -> Model -> IO (Msg)
log level msg model = do
  putStrLn $ show msg
  --putStrLn $ show model
  return Noop

updateWithLog :: (Msg -> Model -> (Model, Cmd Msg)) -> Msg -> Model -> (Model, Cmd Msg)
updateWithLog update msg model = case msg of
  Noop -> update msg model
  _ -> do
    let (model', cmd') = update msg model
    (model', [log Debug msg model] <> cmd')

initialModel :: Model
initialModel =
  Model { _maxNofTasks = 8
        , _buildDir = Path.BuildDir $ Path.relDir "build"
        , _pending = []
        , _inProgress = []
        , _completed = []
        , _objects = Map.empty
        }

main :: IO ()
main = ElmArchitecture.run
  Config { _init = (initialModel, initialCmds)
         , _update = updateWithLog update }
