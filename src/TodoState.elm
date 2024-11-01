module TodoState exposing (..)

import Json.Decode as D
import Json.Encode as E


type TodoState
    = Todo
    | Doing
    | Blocked
    | Done
    | Cancelled


type alias TodoStateFunction =
    TodoState -> TodoState



-- promoteState : TodoStateFunction
-- promoteState state =
--     case state of
--         Todo ->
--             Doing
--         Doing ->
--             Done
--         Done ->
--             Done
-- demoteState : TodoStateFunction
-- demoteState state =
--     case state of
--         Todo ->
--             Todo
--         Doing ->
--             Todo
--         Done ->
--             Doing


encodeTodoState : TodoState -> E.Value
encodeTodoState state =
    case state of
        Todo ->
            E.string "Todo"

        Doing ->
            E.string "Doing"

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

                Blocked ->
                    GT

                Done ->
                    GT

                Cancelled ->
                    EQ
