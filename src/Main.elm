port module Main exposing (..)

import Browser
import File
import File.Download as Download
import File.Select as Select
import Html exposing (Attribute, Html, button, div, img, input, span, table, td, text, th, tr)
import Html.Attributes exposing (autofocus, class, colspan, disabled, height, placeholder, src, style, value, width)
import Html.Events exposing (onClick, onInput)
import Json.Decode as D
import Json.Encode as E
import ModelCore exposing (..)
import Stack
import Task
import TodoItem exposing (..)
import TodoState exposing (..)



-- TODO use bootstrap design
-- TODO render links in task title
-- TODO add filters for state


type alias Model =
    { log : String
    , persistentCore : ModelCore
    , inputBuffer : String
    , editBuffer : String
    , undoStack : Stack.Stack ModelCore
    , redoStack : Stack.Stack ModelCore
    }


type Msg
    = Download
    | FileRequested
    | FileSelected File.File
    | FileLoaded String
    | SetCheckpoint
    | DeleteTodoItem TodoItem
    | InputBufferChange String
    | AddTodoItem
    | Undo
    | Redo
    | Edit TodoItem
    | CancelEdit TodoItem
    | SaveEdit TodoItem
    | EditBufferChange String
    | Promote TodoItem
    | Demote TodoItem
    | ClearOldDone
    | ToggleBlocked TodoItem


port saveToLocalStorage : E.Value -> Cmd msg


main : Program E.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = subscriptions
        }


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
                ""
                Stack.initialise
                Stack.initialise
            , Cmd.none
            )

        Err _ ->
            ( Model
                "Cannot load model from local storage. Starting afresh!"
                (ModelCore 0 [] 0 0)
                ""
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

        cleanerList =
            List.filter (cleaner model.persistentCore.checkpoint) model.persistentCore.todoItems

        cleanButtonText =
            "Clean old (" ++ String.fromInt (List.length cleanerList) ++ ")"
    in
    div [ class "table-container" ]
        [ table []
            ([ tr []
                [ td [ colspan 4 ]
                    [ input
                        [ placeholder "New todo title"
                        , value model.inputBuffer
                        , onInput InputBufferChange
                        , autofocus True
                        , width 200
                        ]
                        []
                    ]
                , td []
                    [ button
                        [ onClick AddTodoItem
                        , disabled (model.inputBuffer == "")
                        ]
                        [ text "Add Todo" ]
                    ]
                ]
             , tr []
                [ th [] [ text "State" ]
                , th [] [ text "🚫" ]
                , th [] [ text "Title" ]
                , th [] [ text "State ++" ]
                , th [] [ text "State --" ]
                , th [] [ text "Action" ]
                ]
             ]
                ++ renderTodoItems model.persistentCore.todoItems model.persistentCore.checkpoint model.editBuffer
            )
        , div [ style "margin-top" "1em" ] [ text "Click Title to edit." ]
        , span []
            [ button [ onClick Undo, disabled (undoStackSize == 0) ] [ text undoButtonText ]
            , button [ onClick Redo, disabled (redoStackSize == 0) ] [ text redoButtonText ]
            , button [ onClick SetCheckpoint ] [ text "Set Checkpoint" ]
            , button [ onClick ClearOldDone, disabled (noCleanbles model) ] [ text cleanButtonText ]
            ]
        , div [] [ text "Database is persisted in this browser only!" ]
        , div [] [ text "Page reload cleans undo history!" ]
        , span []
            [ button [ onClick Download, style "margin-right" "1em", timeToBackup model ] [ text "Download model" ]
            , button [ onClick FileRequested ] [ text "Upload model" ]
            ]
        , div [ style "color" "red" ] [ text model.log ]
        ]


timeToBackup : Model -> Attribute msg
timeToBackup model =
    -- Download button will be red after 10 updates since the last Download. Just to nidge user to backup regularly.
    if model.persistentCore.timestamp - 10 > model.persistentCore.lastBackup then
        style "background" "coral"

    else
        style "" ""


noCleanbles : Model -> Bool
noCleanbles model =
    List.length
        (List.filter (keeper model.persistentCore.checkpoint) model.persistentCore.todoItems)
        == List.length model.persistentCore.todoItems


renderTodoItems : List TodoItem -> Int -> String -> List (Html Msg)
renderTodoItems todoItem cp buffer =
    List.map (renderTodoItem cp buffer) todoItem


renderTodoItem : Int -> String -> TodoItem -> Html Msg
renderTodoItem cp buffer todoItem =
    if todoItem.isEditing then
        tr []
            [ td [] [ renderTodoState todoItem.state ]
            , td [] []
            , td []
                [ input [ value buffer, onInput EditBufferChange ] []
                , button [ onClick (SaveEdit todoItem) ] [ text "Save" ]
                , button [ onClick (CancelEdit todoItem) ] [ text "Cancel" ]
                ]
            , td [] [ button [ onClick (Promote todoItem), disabled True ] [ text "Promote" ] ]
            , td [] [ button [ onClick (Demote todoItem), disabled True ] [ text "Demote" ] ]
            , td [] [ button [ onClick (DeleteTodoItem todoItem), style "background-color" "lightpink", disabled True ] [ text "Delete" ] ]
            ]

    else
        tr []
            [ td [] [ renderTodoState todoItem.state ]
            , td [ onClick (ToggleBlocked todoItem), markBlocked todoItem.isBlocked ] [ text " " ]
            , td [ markTodoItemNew cp todoItem.modificationTime ] [ span [ onClick (Edit todoItem) ] [ text todoItem.title ] ]
            , td [] [ button [ onClick (Promote todoItem), disabled (todoItem.state == Done) ] [ text "Promote" ] ]
            , td [] [ button [ onClick (Demote todoItem), disabled (todoItem.state == Todo) ] [ text "Demote" ] ]
            , td [] [ button [ onClick (DeleteTodoItem todoItem), style "background-color" "lightpink" ] [ text "Delete" ] ]
            ]


markBlocked : Bool -> Attribute msg
markBlocked isBlocked =
    if isBlocked then
        style "background" "orange"

    else
        style "background" "lavender"


renderTodoState : TodoState -> Html Msg
renderTodoState state =
    case state of
        Todo ->
            span [] [ img [ src "images/checkbox.svg", height 20 ] [] ]

        Doing ->
            span [] [ img [ src "images/progress-clock.svg", height 20 ] [] ]

        Done ->
            span [] [ img [ src "images/check-square.svg", height 20 ] [] ]


markTodoItemNew : Int -> Int -> Attribute Msg
markTodoItemNew cp time =
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
            ( { model | persistentCore = setLastBackup model.persistentCore }
            , Download.string
                "scrummer.json"
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

        AddTodoItem ->
            ( { model
                | persistentCore = addNewTodoItem model
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

        DeleteTodoItem todoItem ->
            ( { model
                | persistentCore = deleteTodoItem model todoItem
                , undoStack = Stack.push model.persistentCore model.undoStack
                , redoStack = Stack.initialise
              }
            , Cmd.none
            )

        InputBufferChange buf ->
            ( { model
                | inputBuffer = buf
                , persistentCore = stepLastBackup model.persistentCore
              }
            , Cmd.none
            )

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

        Edit todoItem ->
            ( { model
                | persistentCore = setTodoItemEditing model todoItem True
                , editBuffer = todoItem.title
              }
            , Cmd.none
            )

        CancelEdit todoItem ->
            ( { model
                | persistentCore = setTodoItemEditing model todoItem False
                , editBuffer = todoItem.title
              }
            , Cmd.none
            )

        SaveEdit todoItem ->
            ( { model
                | persistentCore = saveEditedTodoItem model todoItem
                , editBuffer = todoItem.title
                , undoStack = Stack.push model.persistentCore model.undoStack
                , redoStack = Stack.initialise
              }
            , Cmd.none
            )

        EditBufferChange buf ->
            ( { model
                | editBuffer = buf
                , persistentCore = stepLastBackup model.persistentCore
              }
            , Cmd.none
            )

        Promote todoItem ->
            ( { model
                | persistentCore = changeTodoItemStateInModel model todoItem promoteState
                , undoStack = Stack.push model.persistentCore model.undoStack
                , redoStack = Stack.initialise
              }
            , Cmd.none
            )

        Demote todoItem ->
            ( { model
                | persistentCore = changeTodoItemStateInModel model todoItem demoteState
                , undoStack = Stack.push model.persistentCore model.undoStack
                , redoStack = Stack.initialise
              }
            , Cmd.none
            )

        ClearOldDone ->
            ( { model
                | persistentCore = clearOldTodoItemsInModel model
                , undoStack = Stack.push model.persistentCore model.undoStack
                , redoStack = Stack.initialise
              }
            , Cmd.none
            )

        ToggleBlocked todoItem ->
            ( { model
                | persistentCore = toggleBlockedTodoItems model.persistentCore todoItem
                , undoStack = Stack.push model.persistentCore model.undoStack
                , redoStack = Stack.initialise
              }
            , Cmd.none
            )


toggleBlockedTodoItems : ModelCore -> TodoItem -> ModelCore
toggleBlockedTodoItems core todo =
    let
        newTodoItems =
            toggleBlockedAll core.todoItems todo

        toggleBlockedAll : List TodoItem -> TodoItem -> List TodoItem
        toggleBlockedAll todoItems t =
            List.map (blockedToggler t) todoItems

        blockedToggler : TodoItem -> TodoItem -> TodoItem
        blockedToggler theTodo aTodo =
            if theTodo == aTodo then
                { theTodo | isBlocked = not theTodo.isBlocked }

            else
                aTodo
    in
    ModelCore
        core.timestamp
        newTodoItems
        core.checkpoint
        core.lastBackup


setLastBackup : ModelCore -> ModelCore
setLastBackup core =
    { core | lastBackup = core.timestamp }


stepLastBackup : ModelCore -> ModelCore
stepLastBackup core =
    -- need to move the backup in case of rapid updates during buffer change messages
    { core | lastBackup = core.lastBackup + 1 }


clearOldTodoItemsInModel : Model -> ModelCore
clearOldTodoItemsInModel model =
    let
        cleanTodoItems =
            clearTodoItems model.persistentCore.todoItems model.persistentCore.checkpoint
    in
    ModelCore
        model.persistentCore.timestamp
        cleanTodoItems
        model.persistentCore.checkpoint
        model.persistentCore.lastBackup


clearTodoItems : List TodoItem -> Int -> List TodoItem
clearTodoItems todoItems checkpoint =
    List.filter (keeper checkpoint) todoItems


keeper : Int -> TodoItem -> Bool
keeper checkpoint todoItem =
    not (cleaner checkpoint todoItem)


cleaner : Int -> TodoItem -> Bool
cleaner checkpoint todoItem =
    (todoItem.state == Done) && (todoItem.modificationTime <= checkpoint)


changeTodoItemStateInModel : Model -> TodoItem -> TodoStateFunction -> ModelCore
changeTodoItemStateInModel model todoItem stateFun =
    let
        newTodoItems =
            changeTodoItemsState model.persistentCore.todoItems todoItem stateFun model.persistentCore.timestamp
    in
    ModelCore
        model.persistentCore.timestamp
        newTodoItems
        model.persistentCore.checkpoint
        model.persistentCore.lastBackup


changeTodoItemsState : List TodoItem -> TodoItem -> TodoStateFunction -> Int -> List TodoItem
changeTodoItemsState todoItems todoItem stateFun time =
    List.map (changeTodoItemState todoItem stateFun time) todoItems


changeTodoItemState : TodoItem -> TodoStateFunction -> Int -> TodoItem -> TodoItem
changeTodoItemState theTodoItem stateFun time aTodoItem =
    if aTodoItem == theTodoItem then
        { theTodoItem
            | state = stateFun theTodoItem.state
            , modificationTime = time
        }

    else
        aTodoItem


saveEditedTodoItem : Model -> TodoItem -> ModelCore
saveEditedTodoItem model todoItem =
    let
        newTodoItems =
            updateTodoItems model.persistentCore.todoItems todoItem model.editBuffer model.persistentCore.timestamp
    in
    ModelCore
        model.persistentCore.timestamp
        newTodoItems
        model.persistentCore.checkpoint
        model.persistentCore.lastBackup


updateTodoItems : List TodoItem -> TodoItem -> String -> Int -> List TodoItem
updateTodoItems todoItems todoItem title time =
    List.map (updateTodoItem todoItem title time) todoItems


updateTodoItem : TodoItem -> String -> Int -> TodoItem -> TodoItem
updateTodoItem theTodoItem newTitle time aTodoItem =
    if aTodoItem == theTodoItem then
        { theTodoItem | title = newTitle, modificationTime = time, isEditing = False }

    else
        aTodoItem


setTodoItemEditing : Model -> TodoItem -> Bool -> ModelCore
setTodoItemEditing model todoItem state =
    let
        newTodoItems =
            editTodoItem model.persistentCore.todoItems todoItem state
    in
    ModelCore
        model.persistentCore.timestamp
        newTodoItems
        model.persistentCore.checkpoint
        model.persistentCore.lastBackup


editTodoItem : List TodoItem -> TodoItem -> Bool -> List TodoItem
editTodoItem todoItems todoItem state =
    List.map
        (\t ->
            if t == todoItem then
                { t | isEditing = state }

            else
                { t | isEditing = False }
        )
        todoItems


deleteTodoItem : Model -> TodoItem -> ModelCore
deleteTodoItem model todoItem =
    ModelCore
        model.persistentCore.timestamp
        (List.filter (\t -> t /= todoItem) model.persistentCore.todoItems)
        model.persistentCore.checkpoint
        model.persistentCore.lastBackup


addNewTodoItem : Model -> ModelCore
addNewTodoItem model =
    let
        newTodoItems =
            if String.isEmpty model.inputBuffer then
                model.persistentCore.todoItems

            else
                List.append
                    model.persistentCore.todoItems
                    [ TodoItem model.inputBuffer model.persistentCore.timestamp False Todo False ]
    in
    ModelCore
        model.persistentCore.timestamp
        newTodoItems
        model.persistentCore.checkpoint
        model.persistentCore.lastBackup


stepTimestamp : Model -> ModelCore
stepTimestamp model =
    ModelCore
        (model.persistentCore.timestamp + 1)
        model.persistentCore.todoItems
        model.persistentCore.checkpoint
        model.persistentCore.lastBackup


setCheckpoint : Model -> ModelCore
setCheckpoint model =
    ModelCore
        model.persistentCore.timestamp
        model.persistentCore.todoItems
        model.persistentCore.timestamp
        model.persistentCore.lastBackup


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
