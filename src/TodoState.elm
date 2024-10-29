module TodoState exposing (..)

import Json.Decode as D
import Json.Encode as E


type TodoState
    = Todo
    | Doing
    | Done


type alias TodoStateFunction =
    TodoState -> TodoState


promoteState : TodoStateFunction
promoteState state =
    case state of
        Todo ->
            Doing

        Doing ->
            Done

        Done ->
            Done


demoteState : TodoStateFunction
demoteState state =
    case state of
        Todo ->
            Todo

        Doing ->
            Todo

        Done ->
            Doing


encodeTodoState : TodoState -> E.Value
encodeTodoState state =
    case state of
        Todo ->
            E.string "Todo"

        Doing ->
            E.string "Doing"

        Done ->
            E.string "Done"


decodeTodoState : D.Decoder TodoState
decodeTodoState =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "Todo" ->
                        D.succeed Todo

                    "Doing" ->
                        D.succeed Doing

                    "Done" ->
                        D.succeed Done

                    _ ->
                        D.fail "Invalid TodoState"
            )


compareTodoState : TodoState -> TodoState -> Order
compareTodoState s1 s2 =
    case s1 of
        Todo ->
            case s2 of
                Todo ->
                    EQ

                Doing ->
                    LT

                Done ->
                    LT

        Doing ->
            case s2 of
                Todo ->
                    GT

                Doing ->
                    EQ

                Done ->
                    LT

        Done ->
            case s2 of
                Todo ->
                    GT

                Doing ->
                    GT

                Done ->
                    EQ
