module Docker where

import RIO

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import Data.Aeson ((.:))

import qualified Network.HTTP.Simple as HTTP

import qualified Socket

newtype ContainerId = ContainerId Text
  deriving (Eq, Show)

data Service
  = Service
      { createContainer :: CreateContainerOptions -> IO ContainerId
      , startContainer :: ContainerId -> IO ()
      , containerStatus :: ContainerId -> IO ContainerStatus
      , createVolume :: IO Volume
      }

createService :: IO Service
createService = do
  -- Init manager once
  manager <- Socket.newManager "/var/run/docker.sock"

  -- Make Request
  let makeReq :: RequestBuilder
      makeReq path =
        HTTP.defaultRequest
          & HTTP.setRequestPath (encodeUtf8 $ "/v1.40" <> path)
          & HTTP.setRequestManager manager

  pure Service
    { createContainer = createContainer_ makeReq
    , startContainer = startContainer_ makeReq
    , containerStatus = containerStatus_ makeReq
    , createVolume = createVolume_ makeReq
    }

containerIdToText :: ContainerId -> Text
containerIdToText (ContainerId c) = c

data ContainerStatus
  = ContainerRunning
  | ContainerExited ContainerExitCode
  | ContainerOther Text
  deriving (Eq, Show)

data CreateContainerOptions
  = CreateContainerOptions
      { image :: Image
      , script :: Text
      }

createContainer_ :: RequestBuilder -> CreateContainerOptions -> IO ContainerId
createContainer_ makeReq options = do
  manager <- Socket.newManager "/var/run/docker.sock"
  let image = imageToText options.image
  let body = Aeson.object
               [ ("Image", Aeson.toJSON image)
               , ("Tty", Aeson.toJSON True)
               , ("Labels", Aeson.object [("rkci", "")])
               , ("Entrypoint", Aeson.toJSON [Aeson.String "/bin/sh", "-c"])
               , ("Cmd", "echo \"$RKCI_SCRIPT\" | /bin/sh")
               , ("Env", Aeson.toJSON ["RKCI_SCRIPT=" <> options.script])
               ]
  let req = makeReq "/containers/create"
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestBodyJSON body
  let parser = Aeson.withObject "create-container" $ \o -> do
             cId <- o .: "Id"
             pure $ ContainerId cId
  res <- HTTP.httpBS req
  parseResponse res parser

type RequestBuilder = Text -> HTTP.Request

startContainer_ :: RequestBuilder -> ContainerId -> IO ()
startContainer_ makeReq container = do
  let path
        = "/containers/" <> containerIdToText container <> "/start"
  let req = makeReq path
          & HTTP.setRequestMethod "POST"
  void $ HTTP.httpBS req

containerStatus_ :: RequestBuilder -> ContainerId -> IO ContainerStatus
containerStatus_ makeReq container = do
  let parser = Aeson.withObject "container-inspect" $ \o -> do
        state <- o .: "State"
        status <- state .: "Status"
        case status of
          "running" -> pure ContainerRunning
          "exited" -> do
            code <- state .: "ExitCode"
            pure $ ContainerExited (ContainerExitCode code)
          other -> pure $ ContainerOther other

  let req = makeReq $ "/containers/" <> containerIdToText container <> "/json"
  res <- HTTP.httpBS req
  parseResponse res parser

parseResponse
  :: HTTP.Response ByteString
  -> (Aeson.Value -> Aeson.Types.Parser a)
  -> IO a
parseResponse res parser = do
  let result = do
        value <- Aeson.eitherDecodeStrict (HTTP.getResponseBody res)
        Aeson.Types.parseEither parser value
  case result of
    Left e -> throwString e
    Right status -> pure status

createVolume_ :: RequestBuilder -> IO Volume
createVolume_ makeReq = do
  let body = Aeson.object
               [ ("Labels", Aeson.object [("rkci", "")])
               ]
  let req = makeReq "/volumes/create"
        & HTTP.setRequestMethod "POST"
        & HTTP.setRequestBodyJSON body
        
  let parser = Aeson.withObject "create-volume" $ \o -> do
        name <- o .: "Name"
        pure $ Volume name
        
  res <- HTTP.httpBS req
  parseResponse res parser



newtype Image = Image Text
  deriving (Eq, Show)
  
newtype Volume = Volume Text
  deriving (Eq, Show)
  
volumeToText :: Volume -> Text
volumeToText (Volume v) = v

newtype ContainerExitCode = ContainerExitCode Int
  deriving (Eq, Show)

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code

imageToText :: Image -> Text
imageToText (Image image) = image
