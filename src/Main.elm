module Main exposing (..)

import AWSApi exposing (Face, FaceDataPayload, FaceInfo, ImageData, faceDataPayloadDecoder)
import Browser
import Browser.Events
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button)
import Json.Decode
import Loading exposing (LoaderType(..), defaultConfig, render)
import Ports exposing (faceDataReceived, imageLoaded)
import Task



---- MODEL ----


backgroundColor =
    -- 1D3557
    Element.rgb255 29 53 87


lightBackgroundColor =
    -- 457B9D
    Element.rgb255 69 123 157


fillColor =
    -- F1FAEE
    Element.rgb255 241 250 238


lightFillColor =
    -- A8DADC
    Element.rgb255 168 218 220


mainTextColor =
    -- E63946
    Element.rgb255 230 57 70


type alias Flags =
    { startingWidth : Int
    , startingHeight : Int
    }


type alias ScreenSize =
    { width : Int
    , height : Int
    }


type alias Model =
    { imageData : Maybe ImageData
    , faceDataPayload : Maybe FaceDataPayload
    , error : String
    , screenSize : ScreenSize
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { imageData = Maybe.Nothing, faceDataPayload = Maybe.Nothing, error = "", screenSize = { width = flags.startingWidth, height = flags.startingHeight } }, Cmd.none )



---- UPDATE ----


type Msg
    = ImageRequested
    | ImageSelected File
    | ImageLoaded ImageData
    | FaceDataReceived (Result Json.Decode.Error FaceDataPayload)
    | GotNewScreenSize ScreenSize
    | None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ImageRequested ->
            ( model
            , Select.file [ "image/*" ] ImageSelected
            )

        ImageSelected inputFile ->
            ( model
            , Task.perform ImageLoaded
                (Task.map
                    (\content -> ImageData content (File.name inputFile))
                    (File.toUrl inputFile)
                )
            )

        ImageLoaded content ->
            ( { model | imageData = Just content }
            , imageLoaded content
            )

        FaceDataReceived faceDataPayload ->
            case faceDataPayload of
                Ok value ->
                    ( { model | faceDataPayload = Just value }, Cmd.none )

                Err error ->
                    ( { model | error = Json.Decode.errorToString error }, Cmd.none )

        GotNewScreenSize screenSize ->
            ( { model | screenSize = screenSize }, Cmd.none )

        None ->
            ( model, Cmd.none )



---- VIEW ----


header : Element msg
header =
    Element.row
        [ Element.Background.color backgroundColor
        , Element.width Element.fill
        , Element.height <| Element.px 60
        ]
        [ Element.image
            [ Element.height <| Element.px 30
            , Element.paddingEach
                { top = 0
                , right = 0
                , bottom = 0
                , left = 10
                }
            ]
            { src = "logo2.svg"
            , description = "StarFinder logo"
            }
        , Element.el
            [ Element.paddingEach
                { top = 0
                , right = 0
                , bottom = 0
                , left = 10
                }
            , Element.Font.color mainTextColor
            , Element.Font.size 26
            ]
          <|
            Element.text "StarFinder"
        ]


footer : Element msg
footer =
    Element.row
        [ Element.Background.color backgroundColor
        , Element.width Element.fill
        , Element.height <| Element.px 60
        , Element.alignBottom
        ]
        [ Element.el
            [ Element.Font.color mainTextColor
            , Element.Font.size 16
            , Element.width Element.fill
            , Element.Font.center
            ]
          <|
            Element.paragraph []
                [ Element.text "© 2020 Julien Lengrand-Lambert - All rights reserved - "
                , Element.newTabLink [ Element.Font.underline ]
                    { url = "https://elm-lang.org/"
                    , label = Element.text "Built with Elm"
                    }
                ]
        ]


showRecognizedFace : Face -> Element msg
showRecognizedFace face =
    Element.column
        [ Element.Background.color lightFillColor
        , Element.padding 10
        , Element.spacing 10
        , Element.Font.color backgroundColor
        ]
        [ Element.text <| "Found"
        , Element.el [ Element.Font.bold ] <| Element.text face.name
        , Element.row []
            [ Element.text "Confidence level: "
            , Element.el [ Element.Font.bold ] <| Element.text <| String.fromFloat face.matchConfidence
            , Element.text "%"
            ]
        , case List.head face.urls of
            Just link ->
                Element.newTabLink [ Element.Font.color mainTextColor ]
                    { url = "https://" ++ link
                    , label = Element.text "Imdb profile"
                    }

            Maybe.Nothing ->
                Element.none
        ]



-- Element.el
--     [ Element.Background.color lightFillColor
--     , Element.padding 10
--     ]
--     (Element.text <| "Found " ++ face.name)


showRecognizedFaces : List Face -> Element msg
showRecognizedFaces faces =
    Element.el [] <|
        Element.column
            [ Element.spacing 10 ]
        <|
            List.map showRecognizedFace faces


showUnrecognizedFaces : List FaceInfo -> Element msg
showUnrecognizedFaces faceInfos =
    Element.el
        [ Element.Background.color lightFillColor
        , Element.padding 10
        ]
    <|
        Element.paragraph []
            [ Element.text "We also found "
            , Element.el [ Element.Font.bold ] (Element.text <| String.fromInt <| List.length faceInfos)
            , Element.text " faces that we could not recognize."
            ]


view : Model -> Html Msg
view model =
    Element.layout []
        (Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            ]
            [ header
            , Element.row
                [ Element.Background.color fillColor
                , Element.width Element.fill
                , Element.height Element.fill
                ]
                [ case model.imageData of
                    Nothing ->
                        Element.Input.button
                            [ Element.Background.color backgroundColor
                            , Element.focused
                                [ Element.Background.color lightBackgroundColor ]
                            , Element.Font.color mainTextColor
                            , Element.centerX
                            , Element.Border.color backgroundColor
                            , Element.spacing 20
                            , Element.width <| Element.maximum 600 <| Element.px (model.screenSize.width - 20)
                            , Element.height <| Element.maximum 600 <| Element.px (model.screenSize.width - 20)
                            , Element.Border.rounded 50000000
                            ]
                            { onPress = Just ImageRequested
                            , label =
                                Element.el
                                    [ Element.centerX
                                    , Element.centerY
                                    , Element.Font.bold
                                    , Element.Font.size 36
                                    ]
                                <|
                                    Element.text "Upload your picture"
                            }

                    Just imageValue ->
                        case model.faceDataPayload of
                            Nothing ->
                                Element.column
                                    [ Element.width Element.fill
                                    , Element.height Element.fill
                                    ]
                                    [ Element.el
                                        [ Element.centerX
                                        , Element.centerY
                                        , Element.Border.rounded 5000
                                        , Element.Background.image imageValue.content
                                        , Element.width <| Element.px 300
                                        , Element.height <| Element.px 300
                                        ]
                                        (Element.html <| Loading.render Circle { defaultConfig | color = "#E63946", size = 300 } Loading.On)
                                    ]

                            Just faceDataPayload ->
                                Element.column
                                    [ Element.width Element.fill
                                    , Element.height Element.fill
                                    ]
                                    [ Element.image
                                        [ Element.height <| Element.fillPortion 2
                                        , Element.centerY
                                        , Element.centerX
                                        ]
                                        { src = imageValue.content
                                        , description = "Your uploaded images I have done processing on"
                                        }
                                    , Element.column
                                        [ Element.height <| Element.fillPortion 1
                                        , Element.centerY
                                        , Element.centerX
                                        , Element.spacing 10
                                        ]
                                        [ showRecognizedFaces faceDataPayload.faceData.celebrityFaces
                                        , showUnrecognizedFaces faceDataPayload.faceData.unrecognizedFaces
                                        ]
                                    ]
                ]
            , footer
            ]
        )



---- PROGRAM ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ faceDataReceived (Json.Decode.decodeValue faceDataPayloadDecoder >> FaceDataReceived)
        , Browser.Events.onResize (\width height -> GotNewScreenSize { width = width, height = height })
        ]


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
