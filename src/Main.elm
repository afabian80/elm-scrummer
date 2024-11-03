port module Main exposing (main)

import Bootstrap.Alert exposing (simpleDanger, simpleSecondary)
import Bootstrap.Badge as Badge
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Form.Select as Select
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Table as Table
import Bootstrap.Text as Text
import Browser
import File
import File.Download as Download
import File.Select as Select
import Html exposing (Attribute, Html, a, div, h1, p, span, text)
import Html.Attributes exposing (autofocus, class, href, selected, style, value)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E
import ModelCore exposing (..)
import Stack
import Task
import TodoItem exposing (..)
import TodoState exposing (..)



-- TODO remove is_blocked field from todoitem if still unused, make it a FLAG
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
    | ClearOldDone
    | ToggleBlocked TodoItem
    | Sort
    | SelectChange TodoItem String


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
    Grid.container
        []
        [ CDN.stylesheet
        , Grid.row
            []
            [ Grid.col [ Col.textAlign Text.alignLgCenter ] [ h1 [] [ text "Task Manager" ] ] ]
        , Grid.row []
            [ Grid.col [ Col.lg2 ]
                [ Grid.row []
                    [ Grid.col []
                        [ simpleSecondary []
                            [ div [ class "my-2" ] [ text "Database is persisted in this browser only!" ]
                            , div [ class "my-2" ] [ text "Page reload cleans undo history!" ]
                            , div [ class "my-2" ] [ text "Use # or ! in the title to add tags. Links are also rendered. Start a link with slash (`/http`) to render a shorter link." ]
                            ]
                        ]
                    ]
                , Grid.row [] [ Grid.col [] [ renderLog model.log ] ]
                ]
            , Grid.col [ Col.lg8 ]
                [ Form.form []
                    [ InputGroup.config
                        (InputGroup.text
                            [ Input.onInput InputBufferChange
                            , Input.placeholder "Type to add new task..."
                            , Input.value model.inputBuffer
                            , Input.attrs [ autofocus True ]
                            ]
                        )
                        |> InputGroup.small
                        |> InputGroup.successors
                            [ InputGroup.button
                                [ Button.primary
                                , Button.onClick AddTodoItem
                                , Button.disabled (model.inputBuffer == "")
                                ]
                                [ text "Add" ]
                            ]
                        |> InputGroup.view
                    ]
                , p [] []
                , Table.table
                    { options =
                        [ Table.striped
                        , Table.small
                        , Table.bordered
                        ]
                    , thead =
                        Table.simpleThead
                            [ Table.th [] [ text "State" ]
                            , Table.th [] [ text "Change" ]
                            , Table.th [ Table.cellAttr (class "col-md-6") ] [ text "Title (click to edit)" ]
                            , Table.th [] [ text "Actions" ]
                            ]
                    , tbody = Table.tbody [] (renderModel model model.persistentCore.checkpoint model.editBuffer)
                    }
                ]
            , Grid.col [ Col.lg2 ]
                [ Grid.row [] [ Grid.col [] [ text "Actions:" ] ]
                , Grid.row []
                    [ Grid.col []
                        [ Button.button
                            [ Button.primary
                            , Button.small
                            , Button.onClick Sort
                            , Button.attrs [ class "m-1" ]
                            ]
                            [ text "Sort" ]
                        , Button.button
                            [ Button.primary
                            , Button.small
                            , Button.onClick SetCheckpoint
                            , Button.attrs [ class "m-1" ]
                            ]
                            [ text "Checkpoint" ]
                        , Button.button
                            [ Button.danger
                            , Button.small
                            , Button.onClick ClearOldDone
                            , Button.disabled (noCleanbles model)
                            , Button.attrs [ class "m-1" ]
                            ]
                            [ text cleanButtonText ]
                        ]
                    ]
                , Grid.row [] [ Grid.col [] [ text "History:" ] ]
                , Grid.row []
                    [ Grid.col []
                        [ Button.button
                            [ Button.primary
                            , Button.small
                            , Button.onClick Undo
                            , Button.disabled (undoStackSize == 0)
                            , Button.attrs [ class "m-1" ]
                            ]
                            [ text undoButtonText ]
                        , Button.button
                            [ Button.warning
                            , Button.small
                            , Button.onClick Redo
                            , Button.disabled (redoStackSize == 0)
                            , Button.attrs [ class "m-1" ]
                            ]
                            [ text redoButtonText ]
                        ]
                    ]
                , Grid.row [] [ Grid.col [] [ text "Database operations:" ] ]
                , Grid.row []
                    [ Grid.col []
                        [ Button.button
                            [ Button.primary
                            , Button.small
                            , Button.onClick Download
                            , Button.attrs [ class "m-1", timeToBackupBlinker model ]
                            ]
                            [ text "Download"
                            ]
                        , Button.button
                            [ Button.danger
                            , Button.small
                            , Button.onClick FileRequested
                            , Button.attrs [ class "m-1" ]
                            ]
                            [ text "Upload" ]
                        ]
                    ]
                ]
            ]
        ]


timeToBackupBlinker : Model -> Attribute msg
timeToBackupBlinker model =
    if model.persistentCore.timestamp - 10 > model.persistentCore.lastBackup then
        class "blink"

    else
        class ""


renderLog : String -> Html msg
renderLog log =
    if log == "" then
        text ""

    else
        simpleDanger [] [ text log ]


renderModel : Model -> Int -> String -> List (Table.Row Msg)
renderModel model cp buffer =
    List.map (renderTodoItem cp buffer) model.persistentCore.todoItems


renderTodoItem : Int -> String -> TodoItem -> Table.Row Msg
renderTodoItem cp buffer todoItem =
    if todoItem.isEditing then
        Table.tr []
            [ Table.td [] []
            , Table.td [] []
            , Table.td []
                [ Form.form []
                    [ InputGroup.config
                        (InputGroup.text
                            [ Input.onInput EditBufferChange
                            , Input.value buffer
                            ]
                        )
                        |> InputGroup.small
                        |> InputGroup.successors
                            [ InputGroup.button
                                [ Button.primary
                                , Button.onClick (SaveEdit todoItem)
                                , Button.disabled (buffer == "")
                                ]
                                [ text "Save" ]
                            , InputGroup.button
                                [ Button.secondary
                                , Button.onClick (CancelEdit todoItem)
                                ]
                                [ text "Cancel" ]
                            ]
                        |> InputGroup.view
                    ]
                ]
            , Table.td [] []
            ]

    else
        Table.tr
            []
            [ Table.td [] [ renderStatusBadge todoItem ]
            , Table.td []
                [ Select.custom
                    [ Select.id "StatusSelector"
                    , Select.onChange (SelectChange todoItem)
                    , Select.small
                    ]
                    [ Select.item [ value "select" ] [ text "Select..." ]
                    , Select.item [ value "todo" ] [ text "TODO" ]
                    , Select.item [ value "doing" ] [ text "DOING" ]
                    , Select.item [ value "blocked" ] [ text "BLOCKED" ]
                    , Select.item [ value "done" ] [ text "DONE" ]
                    , Select.item [ value "cancelled" ] [ text "CANCELLED" ]
                    ]
                ]
            , Table.td
                (setTitleCellOptions todoItem.state)
                [ if todoItem.state == Done then
                    span []
                        [ --Spinner.spinner [ Spinner.small, Spinner.color Text.secondary ] [ Spinner.srMessage "Doing" ]
                          span [ onClick (Edit todoItem) ]
                            [ renderNewBadge (cp < todoItem.modificationTime)
                            , renderTitle todoItem.title
                            ]
                        ]

                  else
                    span []
                        [ --Spinner.spinner [ Spinner.grow, Spinner.small, Spinner.color Text.secondary ] [ Spinner.srMessage "Doing" ]
                          span [ onClick (Edit todoItem) ]
                            [ renderNewBadge (cp < todoItem.modificationTime)
                            , renderTitle todoItem.title
                            ]
                        ]
                ]
            , Table.td [] [ Button.button [ Button.danger, Button.onClick (DeleteTodoItem todoItem), Button.small ] [ text "Delete" ] ]
            ]


renderTitle : String -> Html Msg
renderTitle title =
    let
        words =
            String.words title

        tagToPrimaryBadge s =
            Badge.badgePrimary [] [ text s ]

        tagToDangerBadge s =
            Badge.badgeDanger [ class "blink" ] [ text s ]

        isTag =
            String.startsWith "#"

        isImportant =
            String.startsWith "!"

        isLink =
            String.startsWith "http"

        isShortLink =
            String.startsWith "/http"

        space =
            text " "

        lastPaths : Int -> String -> String
        lastPaths n s =
            let
                slashParts =
                    String.split "/"

                lastParts =
                    List.drop (List.length (slashParts s) - n) (slashParts s)
            in
            String.join "/" lastParts

        wordToHtml : String -> Html Msg
        wordToHtml s =
            if isTag s then
                span []
                    [ space
                    , tagToPrimaryBadge (String.dropLeft 1 s)
                    , space
                    ]

            else if isImportant s then
                span []
                    [ space
                    , tagToDangerBadge (String.dropLeft 1 s)
                    , space
                    ]

            else if isLink s then
                span []
                    [ space
                    , a [ href s ] [ text s ]
                    , space
                    ]

            else if isShortLink s then
                span []
                    [ space
                    , a [ href (String.dropLeft 1 s) ] [ text (lastPaths 2 s) ]
                    , space
                    ]

            else
                text (" " ++ s ++ " ")
    in
    span [] (List.map wordToHtml words)


setTitleCellOptions : TodoState -> List (Table.CellOption Msg)
setTitleCellOptions state =
    case state of
        Done ->
            [ Table.cellSuccess, Table.cellAttr (style "border" "3px solid green") ]

        Doing ->
            [ Table.cellWarning ]

        Blocked ->
            [ Table.cellDanger ]

        Cancelled ->
            [ Table.cellSuccess, Table.cellAttr (style "border" "3px solid green") ]

        _ ->
            []


renderNewBadge : Bool -> Html msg
renderNewBadge isNew =
    if isNew then
        span [ style "background-color" "white", style "border" "1px solid gray" ] [ text "🌞" ]

    else
        text ""


renderStatusBadge : TodoItem -> Html Msg
renderStatusBadge todoItem =
    case todoItem.state of
        Todo ->
            Badge.badgeSecondary [] [ text "TODO" ]

        Doing ->
            Badge.badgeWarning [] [ text "DOING" ]

        Blocked ->
            Badge.badgeDanger [] [ text "BLOCKED" ]

        Done ->
            Badge.badgeSuccess [] [ text "DONE" ]

        Cancelled ->
            Badge.badgePrimary [] [ text "CANCELLED" ]


noCleanbles : Model -> Bool
noCleanbles model =
    List.length
        (List.filter (keeper model.persistentCore.checkpoint) model.persistentCore.todoItems)
        == List.length model.persistentCore.todoItems


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

        Sort ->
            ( { model
                | persistentCore = sortTodos model.persistentCore
                , undoStack = Stack.push model.persistentCore model.undoStack
                , redoStack = Stack.initialise
              }
            , Cmd.none
            )

        SelectChange todoItem state ->
            ( { model
                | persistentCore = changeTodoItemStateInModel model todoItem state
                , undoStack = Stack.push model.persistentCore model.undoStack
                , redoStack = Stack.initialise
              }
            , Cmd.none
            )


sortTodos : ModelCore -> ModelCore
sortTodos core =
    { core | todoItems = List.sortWith compareTodoItems core.todoItems }


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
    (todoItem.state == Done || todoItem.state == Cancelled) && (todoItem.modificationTime <= checkpoint)


changeTodoItemStateInModel : Model -> TodoItem -> String -> ModelCore
changeTodoItemStateInModel model todoItem state =
    let
        newTodoItems =
            changeTodoItemsState model.persistentCore.todoItems todoItem state model.persistentCore.timestamp
    in
    ModelCore
        model.persistentCore.timestamp
        newTodoItems
        model.persistentCore.checkpoint
        model.persistentCore.lastBackup


changeTodoItemsState : List TodoItem -> TodoItem -> String -> Int -> List TodoItem
changeTodoItemsState todoItems todoItem state time =
    List.map (changeTodoItemState todoItem state time) todoItems


changeTodoItemState : TodoItem -> String -> Int -> TodoItem -> TodoItem
changeTodoItemState theTodoItem state time aTodoItem =
    let
        newState =
            case state of
                "todo" ->
                    Todo

                "doing" ->
                    Doing

                "blocked" ->
                    Blocked

                "done" ->
                    Done

                "cancelled" ->
                    Cancelled

                _ ->
                    Todo
    in
    if aTodoItem == theTodoItem then
        { theTodoItem
            | state = newState
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
