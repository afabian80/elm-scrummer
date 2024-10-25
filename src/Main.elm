port module Main exposing (..)

import Browser
import File
import File.Download as Download
import File.Select as Select
import Html exposing (Attribute, Html, button, div, li, text, ul)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E
import Task



-- TODO use custom text in new task title
-- TODO click to edit task
-- TODO add delete button after task
-- TODO add undo/redo
-- TODO render links in task title


type alias Task =
    { title : String
    , modificationTime : Int
    }


type alias ModelCore =
    { timestamp : Int
    , tasks : List Task
    , checkpoint : Int
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
    | SetCheckpoint


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
    D.map3
        ModelCore
        (D.field "timestamp" D.int)
        (D.field "tasks" (D.list taskDecoder))
        (D.field "checkpoint" D.int)


taskDecoder : D.Decoder Task
taskDecoder =
    D.map2
        Task
        (D.field "title" D.string)
        (D.field "modified" D.int)


encodeModelCore : ModelCore -> E.Value
encodeModelCore modelCore =
    E.object
        [ ( "timestamp", E.int modelCore.timestamp )
        , ( "tasks", E.list encodeTask modelCore.tasks )
        , ( "checkpoint", E.int modelCore.checkpoint )
        ]


encodeTask : Task -> E.Value
encodeTask task =
    E.object
        [ ( "title", E.string task.title )
        , ( "modified", E.int task.modificationTime )
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
            ( Model
                "Cannot load model from local storage. Starting afresh!"
                (ModelCore 0 [] 0)
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick AddAutoTask ] [ text "Add Auto Task" ]
        , button [ onClick SetCheckpoint ] [ text "Set Checkpoint" ]
        , button [ onClick Download ] [ text "Download" ]
        , button [ onClick FileRequested ] [ text "Upload" ]
        , ul [] (renderTasks model.persistentCore.tasks model.persistentCore.checkpoint)
        , div [] [ text ("Timestamp: " ++ String.fromInt model.persistentCore.timestamp) ]
        , div [] [ text ("Checkpoint: " ++ String.fromInt model.persistentCore.checkpoint) ]
        , div [ style "color" "red" ] [ text model.log ]
        ]


renderTasks : List Task -> Int -> List (Html Msg)
renderTasks tasks cp =
    List.map (renderTask cp) tasks


renderTask : Int -> Task -> Html Msg
renderTask cp task =
    li [ markTaskNew cp task.modificationTime ]
        [ text
            (task.title
                ++ " ("
                ++ String.fromInt task.modificationTime
                ++ ")"
            )
        ]


markTaskNew : Int -> Int -> Attribute Msg
markTaskNew cp time =
    if time >= cp then
        style "background" "lightgreen"

    else
        style "" ""


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
    ( newModel
    , Cmd.batch
        [ saveToLocalStorage (encodeModelCore newModel.persistentCore)
        , cmds
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg modelOriginal =
    let
        --clear model log
        model =
            { modelOriginal
                | log = ""
                , persistentCore = stepTimestamp modelOriginal
            }
    in
    case msg of
        Download ->
            ( model
            , Download.string
                "akos.json"
                "text/json"
                (E.encode 4 (encodeModelCore model.persistentCore))
            )

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

        SetCheckpoint ->
            ( { model | persistentCore = setCheckpoint model }, Cmd.none )


addNewTask : Model -> ModelCore
addNewTask model =
    ModelCore
        model.persistentCore.timestamp
        (List.append
            model.persistentCore.tasks
            [ Task "hello" model.persistentCore.timestamp ]
        )
        model.persistentCore.checkpoint


stepTimestamp : Model -> ModelCore
stepTimestamp model =
    ModelCore
        (model.persistentCore.timestamp + 1)
        model.persistentCore.tasks
        model.persistentCore.checkpoint


setCheckpoint : Model -> ModelCore
setCheckpoint model =
    ModelCore
        model.persistentCore.timestamp
        model.persistentCore.tasks
        model.persistentCore.timestamp


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
