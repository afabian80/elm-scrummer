port module Main exposing (..)

import Browser
import File
import File.Download as Download
import File.Select as Select
import Html exposing (Attribute, Html, button, div, input, li, text, ul)
import Html.Attributes exposing (autofocus, disabled, placeholder, style, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as D
import Json.Encode as E
import Stack
import Task



-- TODO click to edit task
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
    , inputBuffer : String
    , undoStack : Stack.Stack ModelCore
    , redoStack : Stack.Stack ModelCore
    }


type Msg
    = Download
    | FileRequested
    | FileSelected File.File
    | FileLoaded String
    | SetCheckpoint
    | DeleteTask Task
    | InputBufferChange String
    | AddTask
    | Undo
    | Redo


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
            ( Model
                ""
                modelCore
                ""
                Stack.initialise
                Stack.initialise
            , Cmd.none
            )

        Err _ ->
            ( Model
                "Cannot load model from local storage. Starting afresh!"
                (ModelCore 0 [] 0)
                ""
                Stack.initialise
                Stack.initialise
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        undoStackSize =
            List.length (Stack.toList model.undoStack)

        undoStackSizeStr =
            String.fromInt undoStackSize

        undoButtonText =
            "Undo (" ++ undoStackSizeStr ++ ")"

        redoStackSize =
            List.length (Stack.toList model.redoStack)

        redoStackSizeStr =
            String.fromInt redoStackSize

        redoButtonText =
            "Redo (" ++ redoStackSizeStr ++ ")"
    in
    div []
        [ -- Cannot handle Enter directly, use something like [ input [ onInput InputChanged, onKeyDown (\e -> if e.keyCode == 13 then SubmitForm else InputChanged e.targetValue) ] []
          input
            [ placeholder "New task title"
            , value model.inputBuffer
            , onInput InputBufferChange
            , autofocus True
            ]
            []
        , button
            [ onClick AddTask
            , disabled (model.inputBuffer == "")
            ]
            [ text "Add Task" ]
        , button [ onClick SetCheckpoint ] [ text "Set Checkpoint" ]
        , button [ onClick Download ] [ text "Download" ]
        , button [ onClick FileRequested ] [ text "Upload" ]
        , button
            [ onClick Undo
            , disabled (undoStackSize == 0)
            ]
            [ text undoButtonText ]
        , button
            [ onClick Redo
            , disabled (redoStackSize == 0)
            ]
            [ text redoButtonText ]
        , ul [] (renderTasks model.persistentCore.tasks model.persistentCore.checkpoint)
        , div [] [ text ("Timestamp: " ++ String.fromInt model.persistentCore.timestamp) ]
        , div [] [ text ("Checkpoint: " ++ String.fromInt model.persistentCore.checkpoint) ]
        , div [] [ text ("Input buffer: " ++ model.inputBuffer) ]
        , div [] [ text ("Undo stack size: " ++ undoStackSizeStr) ]
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
        , button [ onClick (DeleteTask task) ] [ text "Delete" ]
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
        --clear model log, and step timestamp
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

        AddTask ->
            ( { model
                | persistentCore = addNewTask model
                , inputBuffer = ""
                , undoStack = Stack.push model.persistentCore model.undoStack
                , redoStack = Stack.initialise
              }
            , Cmd.none
            )

        SetCheckpoint ->
            ( { model
                | persistentCore = setCheckpoint model
                , undoStack = Stack.push model.persistentCore model.undoStack
              }
            , Cmd.none
            )

        DeleteTask task ->
            ( { model
                | persistentCore = deleteTask model task
                , undoStack = Stack.push model.persistentCore model.undoStack
                , redoStack = Stack.initialise
              }
            , Cmd.none
            )

        InputBufferChange buf ->
            ( { model | inputBuffer = buf }, Cmd.none )

        Undo ->
            let
                ( mCore, newStack ) =
                    Stack.pop model.undoStack
            in
            case mCore of
                Just core ->
                    ( { model
                        | persistentCore = core
                        , undoStack = newStack
                        , redoStack = Stack.push model.persistentCore model.redoStack
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        Redo ->
            let
                ( mCore, newStack ) =
                    Stack.pop model.redoStack
            in
            case mCore of
                Just core ->
                    ( { model
                        | persistentCore = core
                        , redoStack = newStack
                        , undoStack = Stack.push model.persistentCore model.undoStack
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )


deleteTask : Model -> Task -> ModelCore
deleteTask model task =
    ModelCore
        model.persistentCore.timestamp
        (List.filter (\t -> t /= task) model.persistentCore.tasks)
        model.persistentCore.checkpoint


addNewTask : Model -> ModelCore
addNewTask model =
    let
        newTasks =
            if String.isEmpty model.inputBuffer then
                model.persistentCore.tasks

            else
                List.append
                    model.persistentCore.tasks
                    [ Task model.inputBuffer model.persistentCore.timestamp ]
    in
    ModelCore
        model.persistentCore.timestamp
        newTasks
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
