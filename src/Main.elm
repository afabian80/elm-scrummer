port module Main exposing (..)

import Browser
import File
import File.Download as Download
import File.Select as Select
import Html exposing (Html, button, div, li, text, ul)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E
import Task


type alias ModelCore =
    { tasks : List String
    }


type alias Model =
    { log : String
    , persistentCore : ModelCore
    }


type Msg
    = Download
    | FileRequested
    | FileSelected File.File
    | FileLoaded String
    | AddAutoTask


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
        (D.field "tasks" (D.list D.string))


encodeModelCore : ModelCore -> E.Value
encodeModelCore modelCore =
    E.object
        [ ( "tasks", E.list E.string modelCore.tasks )
        ]


init : E.Value -> ( Model, Cmd Msg )
init flag =
    let
        core =
            D.decodeValue modelCoreDecoder flag
    in
    case core of
        Ok modelCore ->
            ( Model "" modelCore, Cmd.none )

        Err _ ->
            ( Model "Cannot load model from local storage. Starting afresh!" (ModelCore []), Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ ul [] (renderTasks model.persistentCore.tasks)
        , button [ onClick AddAutoTask ] [ text "Add Auto Task" ]
        , button [ onClick Download ] [ text "Download" ]
        , button [ onClick FileRequested ] [ text "Upload" ]
        , div [] [ text model.log ]
        ]


renderTasks : List String -> List (Html Msg)
renderTasks tasks =
    List.map renderTask tasks


renderTask : String -> Html Msg
renderTask task =
    li [] [ text task ]


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
            { modelOriginal
                | log = ""
            }
    in
    case msg of
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

        AddAutoTask ->
            ( { model
                | persistentCore = addNewTask model
              }
            , Cmd.none
            )


addNewTask : Model -> ModelCore
addNewTask model =
    ModelCore (List.append model.persistentCore.tasks [ "hello-" ])


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
