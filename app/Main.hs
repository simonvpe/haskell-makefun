{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad.Except (ExceptT, runExceptT)
import Data.Map.Strict (Map)
import Data.Path ((</>), (<.>))
import ElmArchitecture (Config(_init, _update, Config), Cmd)
import Prelude hiding (init, log)
import qualified Data.Hash as Hash
import qualified Data.Map.Strict as Map
import qualified Data.Path as Path
import qualified Effect.Compile as Compile
import qualified Effect.CompileSpec as CompileSpec
import qualified ElmArchitecture
import Debug.Trace

-- |
-- | MODEL
-- |

data Job = Job { taskId :: TaskId
               , task :: Task
               , depends :: [TaskId]
               } deriving (Show)

data Model =
  Model { maxNofTasks :: Int
        , buildDir :: Path.BuildDir
        , pending :: [Job]
        , inProgress :: [Job]
        , completed :: [TaskId]
        } deriving (Show)

-- |
-- | EVENTS
-- |

data LogLevel
  = Debug
  | Info
  | Warning
  | Error
  deriving (Show)

newtype TaskId = TaskId Int deriving (Show, Eq)

data Task
  = Compile Path.SourceFile
  deriving (Show)

data Msg
  = Noop
  | AddTask Job
  | FinalizeTask Job
  | FatalError String
  deriving (Show)

compile :: Job -> Path.BuildDir -> Path.SourceFile -> IO Msg
compile job buildDir sourceFile = do
  spec <- runExceptT $ CompileSpec.make buildDir sourceFile
  case spec of
    Left err -> return $ FatalError $ show err
    Right mspec -> case mspec of
      Nothing -> return $ FinalizeTask job
      Just spec -> do
        result <- runExceptT $ Compile.compile spec
        case result of
          Left err    -> return $ FatalError $ show err
          Right False -> return $ FatalError "compilation failed"          
          Right True  -> return $ FinalizeTask job

runTask :: Job -> Model -> (Model, Cmd Msg)
runTask job model = (model', cmds)
  where model' = model { inProgress = (inProgress model) <> [job] }
        cmds = case (task job) of
          Compile sourceFile -> [compile job (buildDir model) sourceFile]  

deferTask :: Job -> Model -> (Model, Cmd Msg)
deferTask job model = (model', [])
  where  model' = model { pending = (pending model) <> [job] }
      
finalizeTask :: Job -> Model -> (Model, Cmd Msg)
finalizeTask job model = (model'', cmds)
  where model' =  model { inProgress = filter ((/=) (taskId job) . taskId) (inProgress model)
                        , completed = (completed model) <> [taskId job] }
        (model'', cmds) = case pending model of
          (x:xs) -> (model' { pending = xs }, [return $ AddTask job])
          _      -> (model', [])

update :: Msg -> Model -> (Model, Cmd Msg)
update msg model = do
  case msg of
    Noop ->
      (model, [])

    AddTask job ->
      if (length $ inProgress model) < (maxNofTasks model) then
        runTask job model
      else
        deferTask job model

    FinalizeTask taskId ->
      finalizeTask taskId model
      
    FatalError msg ->
      (model, [])




addCompileTask :: TaskId -> [TaskId] -> Path.SourceFile -> IO Msg
addCompileTask taskId depends sourceFile = pure $ AddTask $ Job {taskId, depends, task = Compile sourceFile}

initialCmds :: Cmd Msg
initialCmds =
  [ addCompileTask (TaskId 1) [] (Path.SourceFile $ Path.relDir "cpp" </> Path.relFile "test" <.> "cpp")
  , addCompileTask (TaskId 2) [] (Path.SourceFile $ Path.relDir "cpp" </> Path.relFile "test2" <.> "cpp")
  , addCompileTask (TaskId 3) [] (Path.SourceFile $ Path.relDir "cpp" </> Path.relFile "test3" <.> "cpp")
  , addCompileTask (TaskId 4) [] (Path.SourceFile $ Path.relDir "cpp" </> Path.relFile "test4" <.> "cpp")
  , addCompileTask (TaskId 5) [] (Path.SourceFile $ Path.relDir "cpp" </> Path.relFile "test5" <.> "cpp")
  , addCompileTask (TaskId 6) [] (Path.SourceFile $ Path.relDir "cpp" </> Path.relFile "test6" <.> "cpp")
  , addCompileTask (TaskId 7) [] (Path.SourceFile $ Path.relDir "cpp" </> Path.relFile "test7" <.> "cpp")
  , addCompileTask (TaskId 8) [] (Path.SourceFile $ Path.relDir "cpp" </> Path.relFile "test8" <.> "cpp")      
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
  Model { maxNofTasks = 4
        , buildDir = Path.BuildDir $ Path.relDir "build"
        , pending = []
        , inProgress = []
        , completed = []
        }

main :: IO ()
main = ElmArchitecture.run
  Config { _init = (initialModel, initialCmds)
         , _update = updateWithLog update }
