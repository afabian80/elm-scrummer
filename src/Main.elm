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


modelDecoder : D.Decoder Model
modelDecoder =
    D.map3
        Model
        (D.field "data" D.int)
        (D.field "log" D.string)
        (D.field "persistent_core" modelCoreDecoder)


encodeModel : Model -> E.Value
encodeModel model =
    E.object
        [ ( "data", E.int model.data )
        , ( "log", E.string model.log )
        , ( "persistent_core", encodeModelCore model.persistentCore )
        ]


encodeModelCore : ModelCore -> E.Value
encodeModelCore modelCore =
    E.object
        [ ( "pdata", E.string modelCore.pdata )
        ]


init : E.Value -> ( Model, Cmd Msg )
init flag =
    let
        f =
            D.decodeValue modelDecoder flag
    in
    case f of
        Ok model ->
            ( model, Cmd.none )

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
    ( newModel, Cmd.batch [ saveToLocalStorage (encodeModel newModel), cmds ] )


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
            ( model, Download.string "akos.json" "text/json" (E.encode 4 (encodeModel model)) )

        FileRequested ->
            ( model, Select.file [ "text/json" ] FileSelected )

        FileSelected file ->
            ( model, Task.perform FileLoaded (File.toString file) )

        FileLoaded text ->
            let
                modelResult =
                    D.decodeString modelDecoder text
            in
            case modelResult of
                Ok m ->
                    ( m, Cmd.none )

                Err e ->
                    ( { model | log = D.errorToString e }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
