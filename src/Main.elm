port module Main exposing (..)

import Browser
import File
import File.Download as Download
import File.Select as Select
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E
import Task


type alias ModelCore =
    { pdata : String
    }


type alias Model =
    { data : Int
    , log : String
    , persistentCore : ModelCore
    }


type Msg
    = Increment
    | Download
    | FileRequested
    | FileSelected File.File
    | FileLoaded String


port saveToLocalStorage : E.Value -> Cmd msg


main : Program E.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = subscriptions
        }


modelCoreDecoder : D.Decoder ModelCore
modelCoreDecoder =
    D.map
        ModelCore
        (D.field "pdata" D.string)


encodeModelCore : ModelCore -> E.Value
encodeModelCore modelCore =
    E.object
        [ ( "pdata", E.string modelCore.pdata )
        ]


init : E.Value -> ( Model, Cmd Msg )
init flag =
    let
        core =
            D.decodeValue modelCoreDecoder flag
    in
    case core of
        Ok modelCore ->
            ( Model 0 "" modelCore, Cmd.none )

        Err e ->
            ( Model 0 (D.errorToString e) (ModelCore ""), Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ text ("Data: " ++ String.fromInt model.data)
        , text ("Persistent data: " ++ model.persistentCore.pdata)
        , button [ onClick Increment ] [ text "Increment" ]
        , button [ onClick Download ] [ text "Download" ]
        , button [ onClick FileRequested ] [ text "Upload" ]
        , text model.log
        ]


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
    ( newModel, Cmd.batch [ saveToLocalStorage (encodeModelCore newModel.persistentCore), cmds ] )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg modelOriginal =
    let
        --clear model log
        model =
            { modelOriginal | log = "" }
    in
    case msg of
        Increment ->
            ( { model
                | data = model.data + 1
                , persistentCore = ModelCore ("--- " ++ String.fromInt (model.data + 1))
              }
            , Cmd.none
            )

        Download ->
            ( model, Download.string "akos.json" "text/json" (E.encode 4 (encodeModelCore model.persistentCore)) )

        FileRequested ->
            ( model, Select.file [ "text/json" ] FileSelected )

        FileSelected file ->
            ( model, Task.perform FileLoaded (File.toString file) )

        FileLoaded text ->
            let
                modelCoreResult =
                    D.decodeString modelCoreDecoder text
            in
            case modelCoreResult of
                Ok core ->
                    ( { model | persistentCore = core }, Cmd.none )

                Err e ->
                    ( { model | log = D.errorToString e }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
