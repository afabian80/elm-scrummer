module TodoState exposing (..)

import Json.Decode as D
import Json.Encode as E


type TodoState
    = Todo
    | Doing
    | Waiting
    | Blocked
    | Done
    | Cancelled


type alias TodoStateFunction =
    TodoState -> TodoState


encodeTodoState : TodoState -> E.Value
encodeTodoState state =
    case state of
        Todo ->
            E.string "Todo"

        Doing ->
            E.string "Doing"

        Waiting ->
            E.string "Waiting"

        Blocked ->
            E.string "Blocked"

        Done ->
            E.string "Done"

        Cancelled ->
            E.string "Cancelled"


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

                    "Waiting" ->
                        D.succeed Waiting

                    "Blocked" ->
                        D.succeed Blocked

                    "Done" ->
                        D.succeed Done

                    "Cancelled" ->
                        D.succeed Cancelled

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

                Waiting ->
                    LT

                Blocked ->
                    LT

                Done ->
                    LT

                Cancelled ->
                    LT

        Doing ->
            case s2 of
                Todo ->
                    GT

                Doing ->
                    EQ

                Waiting ->
                    LT

                Blocked ->
                    LT

                Done ->
                    LT

                Cancelled ->
                    LT

        Waiting ->
            case s2 of
                Todo ->
                    GT

                Doing ->
                    GT

                Waiting ->
                    EQ

                Blocked ->
                    LT

                Done ->
                    LT

                Cancelled ->
                    LT

        Blocked ->
            case s2 of
                Todo ->
                    GT

                Doing ->
                    GT

                Waiting ->
                    GT

                Blocked ->
                    EQ

                Done ->
                    LT

                Cancelled ->
                    LT

        Done ->
            case s2 of
                Todo ->
                    GT

                Doing ->
                    GT

                Waiting ->
                    GT

                Blocked ->
                    GT

                Done ->
                    EQ

                Cancelled ->
                    LT

        Cancelled ->
            case s2 of
                Todo ->
                    GT

                Doing ->
                    GT

                Waiting ->
                    GT

                Blocked ->
                    GT

                Done ->
                    GT

                Cancelled ->
                    EQ
