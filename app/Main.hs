{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Lens hiding ((:>), at, (<.>), _init)
import Data.Map.Strict (Map)
import Data.Path ((</>), (<.>))
import Data.List (delete, (\\))
import ElmArchitecture (Config(_init, _update, Config), Cmd)
import Prelude hiding (init, log)
import qualified Data.Hash as Hash
import qualified Data.Map.Strict as Map
import qualified Data.Path as Path
import qualified Effect.Compile as Compile
import qualified Effect.CompileSpec as CompileSpec
import qualified ElmArchitecture
import qualified Toolchain.Gcc.DependencyParser as DependencyParser
import Debug.Trace

-- |
-- | MODEL
-- |

data Task
  = Compile Path.SourceFile
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
  | AddTask Job
  | FinalizeTask Job
  | FatalError String
  deriving (Show)

compile :: Job -> Path.BuildDir -> Path.SourceFile -> IO Msg
compile job buildDir sourceFile = do
  spec <- runExceptT $ CompileSpec.make DependencyParser.parse buildDir sourceFile
  case spec of
    Left err -> return $ FatalError $ show err
    Right mspec -> case mspec of
      Nothing -> return $ FinalizeTask job
      Just spec -> do
        result <- runExceptT $ Compile.compile DependencyParser.parse spec
        case result of
          Left err    -> return $ FatalError $ show err
          Right False -> return $ FatalError "compilation failed"
          Right True  -> return $ FinalizeTask job

linkStaticLibrary :: Job -> Path.BuildDir -> [Path.SourceFile] -> Path.StaticLibraryFile -> IO Msg
linkStaticLibrary job buildDir sources (Path.StaticLibraryFile library) = do
  putStrLn $ "Linking " <> (show $ (\(Path.SourceFile s) -> Path.toString s) <$> sources) <> " -> " <> (show $ Path.toString library)
  return $ FinalizeTask job

runJob :: Job -> Model -> Cmd Msg
runJob job model =
  case job^.task of
    Compile sourceFile               -> [compile job (model^.buildDir) sourceFile]
    LinkStaticLibrary sources library -> [linkStaticLibrary job (model^.buildDir) sources library]

update :: Msg -> Model -> (Model, Cmd Msg)
update msg model = do
  case msg of
    Noop ->
      (model, [])

    AddTask job ->
      if (model^.inProgress^.to length) < (model^.maxNofTasks) && (job^.depends^.to length) == 0 then
        (inProgress %~ ((++) [job]) $ model, runJob job model)
      else
        (pending %~ ((++) [job]) $ model, [])

    FinalizeTask job -> do
      let pendingRevised = (depends %~ (delete $ job^.taskId)) <$> model^.pending
      let model' = pending .~ []
                   $ inProgress %~ (delete job)
                   $ completed %~ ((++) [job^.taskId])
                   $ model
      (model', return . AddTask <$> pendingRevised)
  
    FatalError msg ->
      (model, [])




addCompileTask :: TaskId -> [TaskId] -> Path.SourceFile -> IO Msg
addCompileTask _taskId _depends sourceFile = pure $ AddTask $ Job {_taskId, _depends, _task = Compile sourceFile}

addLinkStaticLibraryTask :: TaskId -> [TaskId] -> IO Msg
addLinkStaticLibraryTask _taskId _depends = pure $ AddTask $ Job {_taskId, _depends, _task = LinkStaticLibrary [] $ Path.StaticLibraryFile (Path.relDir "build" </> Path.relFile "lib" <.> "a")}

initialCmds :: Cmd Msg
initialCmds =
  [ addCompileTask (TaskId 1) [] (Path.SourceFile $ Path.relDir "cpp" </> Path.relFile "test" <.> "cpp")
  , addCompileTask (TaskId 2) [] (Path.SourceFile $ Path.relDir "cpp" </> Path.relFile "test2" <.> "cpp")
  , addCompileTask (TaskId 3) [] (Path.SourceFile $ Path.relDir "cpp" </> Path.relFile "test3" <.> "cpp")
  , addCompileTask (TaskId 4) [] (Path.SourceFile $ Path.relDir "cpp" </> Path.relFile "test4" <.> "cpp")
  , addLinkStaticLibraryTask (TaskId 5) ([TaskId 10] <> (TaskId <$> [1, 2, 3, 4]))
  , addCompileTask (TaskId 6) [] (Path.SourceFile $ Path.relDir "cpp" </> Path.relFile "test5" <.> "cpp")
  , addCompileTask (TaskId 7) [] (Path.SourceFile $ Path.relDir "cpp" </> Path.relFile "test6" <.> "cpp")
  , addCompileTask (TaskId 8) [] (Path.SourceFile $ Path.relDir "cpp" </> Path.relFile "test7" <.> "cpp")
  , addCompileTask (TaskId 9) [] (Path.SourceFile $ Path.relDir "cpp" </> Path.relFile "test8" <.> "cpp")
  , addLinkStaticLibraryTask (TaskId 10) (TaskId <$> [6, 7, 8, 9])  
  ]

















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
  Model { _maxNofTasks = 4
        , _buildDir = Path.BuildDir $ Path.relDir "build"
        , _pending = []
        , _inProgress = []
        , _completed = []
        }

main :: IO ()
main = ElmArchitecture.run
  Config { _init = (initialModel, initialCmds)
         , _update = update }
