port module Ports exposing (..)

import Json.Encode
import AWSApi exposing (ImageData)

port imageLoaded : ImageData -> Cmd msg

-- port faceDataReceived : (FaceData -> msg) -> Sub msg
port faceDataReceived : (Json.Encode.Value -> msg) -> Sub msg