module Core where

import RIO

import qualified RIO.Map as Map
import qualified RIO.List as List

import qualified Docker

data Pipeline
  = Pipeline
      { steps :: NonEmpty Step
      }
  deriving (Eq, Show)

data Step
  = Step
      { name :: StepName
      , commands :: NonEmpty Text
      , image :: Docker.Image
      }
  deriving (Eq, Show)

data Build
  = Build
      { pipeline :: Pipeline
      , state :: BuildState
      , completedSteps :: Map StepName StepResult
      }
  deriving (Eq, Show)

data StepResult
  = StepFailed Docker.ContainerExitCode
  | StepSucceeded
  deriving (Eq, Show)

newtype ContainerExitCode = ContainerExitCode Int
  deriving (Eq, Show)

exitCodeToStepResult :: Docker.ContainerExitCode -> StepResult
exitCodeToStepResult exit =
  if Docker.exitCodeToInt exit == 0
    then StepSucceeded
    else StepFailed exit

{- Brief explanation: if any steps failed, then we can consider the build to have failed as well. 
Otherwise we go through the steps in the pipeline and find one which hasn’t run yet (not in the completedSteps Map). 
If we can’t find a step, then they all succeeded so the build is successful.
-}
buildHasNextStep :: Build -> Either BuildResult Step
buildHasNextStep build =
  if allSucceeded
    then case nextStep of
      Just step -> Right step
      Nothing -> Left BuildSucceeded
    else Left BuildFailed
  where
    allSucceeded = List.all ((==) StepSucceeded) build.completedSteps
    nextStep = List.find f build.pipeline.steps
    f step = not $ Map.member step.name build.completedSteps


data BuildState
  = BuildReady
  | BuildRunning BuildRunningState
  | BuildFinished BuildResult
  deriving (Eq, Show)

data BuildRunningState
  = BuildRunningState
      { step :: StepName
      }
  deriving (Eq, Show)

data BuildResult
  = BuildSucceeded
  | BuildFailed
  deriving (Eq, Show)

progress :: Docker.Service -> Build -> IO Build
progress docker build =
  case build.state of
    BuildReady ->
      case buildHasNextStep build of
        Left result ->
          pure $ build{state = BuildFinished result}
        Right step -> do
          let options = Docker.CreateContainerOptions step.image
          container <- docker.createContainer options
          docker.startContainer container
          let s = BuildRunningState { step = step.name }
          pure $ build{state = BuildRunning s}
    BuildRunning state -> do
      -- We'll assume the container exited with a 0 status code.
      let exit = Docker.ContainerExitCode 0
          result = exitCodeToStepResult exit

      pure build
        { state = BuildReady
        , completedSteps
          = Map.insert state.step result build.completedSteps
        }
    BuildFinished _ ->
      pure build

newtype StepName = StepName Text
  deriving (Eq, Show, Ord)

stepNameToText :: StepName -> Text
stepNameToText (StepName step) = step
