module Main where

import RIO
import Core
  
-- Helper functions
makeStep :: Text -> Text -> [Text] -> Step
makeStep name image commands
  = Step
      { name = StepName name
      , image = Image image
      , commands = commands
      }

makePipeline :: [Step] -> Pipeline
makePipeline steps =
  Pipeline { steps = steps }

-- Test values
testPipeline :: Pipeline
testPipeline = makePipeline
  [ makeStep "First step" "ubuntu" ["date"]
  , makeStep "Second step" "ubuntu" ["uname -r"]
  ]

testBuild :: Build
testBuild = Build
  { pipeline = testPipeline
  , state = BuildReady
  }

main :: IO ()
main = pure ()