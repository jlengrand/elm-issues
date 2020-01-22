module AWSApi exposing (..)

import Json.Decode.Pipeline exposing (required)
import Json.Decode exposing (string, float, succeed, list, Decoder)

type alias Landmark = 
    {
        name: String
        , x : Float
        , y : Float
    }

type alias BoundingBox = 
    {
        height : Float
        , left : Float
        , top : Float
        , width : Float
    }

type alias Quality = 
    {
        brightness : Float
        , sharpness: Float

    }

type alias Pose = 
    {
        roll: Float
        , yaw : Float
        , pitch : Float
    }

type alias FaceInfo = 
    {
        boundingBox : BoundingBox
        , confidence : Float
        , landmarks : List Landmark
        , pose : Pose
        , quality : Quality
    }

type alias Face = 
    {
        -- BoundingBox
        name : String
        , urls : List String
        , matchConfidence : Float
        , id : String
        , faceInfo: FaceInfo
    }

type alias FaceData =
    {
        celebrityFaces: List Face
        , unrecognizedFaces : List FaceInfo
    }

type alias S3Data = 
    {
        bucket : String, 
        key : String, 
        location: String 
    }

type alias FaceDataPayload = 
    { 
        s3Data : S3Data
        , faceData : FaceData
    }

type alias ImageData =
    { content : String
    , name : String
    }



poseDecoder : Decoder Pose
poseDecoder =
    succeed Pose
        |> required "Roll" float
        |> required "Yaw" float
        |> required "Pitch" float

qualityDecoder : Decoder Quality
qualityDecoder =
    succeed Quality
        |> required "Brightness" float
        |> required "Sharpness" float

boundingBoxDecoder : Decoder BoundingBox
boundingBoxDecoder =
    succeed BoundingBox
        |> required "Height" float
        |> required "Left" float
        |> required "Top" float
        |> required "Width" float

landmarkDecoder : Decoder Landmark
landmarkDecoder =
    succeed Landmark
        |> required "Type" string
        |> required "X" float
        |> required "Y" float

landmarkListDecoder : Decoder (List Landmark)
landmarkListDecoder =
    list landmarkDecoder

faceInfoDecoder : Decoder FaceInfo
faceInfoDecoder =
    succeed FaceInfo
        |> required "BoundingBox" boundingBoxDecoder
        |> required "Confidence" float
        |> required "Landmarks" landmarkListDecoder
        |> required "Pose" poseDecoder
        |> required "Quality" qualityDecoder

urlListDecoder : Decoder (List String)
urlListDecoder =
    list string

faceDecoder : Decoder Face
faceDecoder = 
    succeed Face
        |> required "Name" string
        |> required "Urls" urlListDecoder
        |> required "MatchConfidence" float
        |> required "Id" string
        |> required "Face" faceInfoDecoder


faceListDecoder : Decoder (List Face)
faceListDecoder =
    list faceDecoder

faceInfoListDecoder : Decoder (List FaceInfo)
faceInfoListDecoder =
    list faceInfoDecoder

faceDataDecoder : Decoder FaceData
faceDataDecoder = 
    succeed FaceData
        |> required "CelebrityFaces" faceListDecoder
        |> required "UnrecognizedFaces" faceInfoListDecoder


s3DataDecoder : Decoder S3Data
s3DataDecoder = 
    succeed S3Data
        |> required "Bucket" string
        |> required "Key" string
        |> required "Location" string

faceDataPayloadDecoder : Decoder FaceDataPayload
faceDataPayloadDecoder =
    succeed FaceDataPayload
        |> required "s3Data" s3DataDecoder
        |> required "faceData" faceDataDecoder
        