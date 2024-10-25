module TodoState exposing (..)

import Json.Decode as D
import Json.Encode as E


type TodoState
    = Todo
    | Doing
    | Done


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
