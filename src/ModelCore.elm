module ModelCore exposing (..)

import Json.Decode as D
import Json.Encode as E
import TodoItem exposing (..)


type alias ModelCore =
    { timestamp : Int
    , todoItems : List TodoItem
    , checkpoint : Int
    }


modelCoreDecoder : D.Decoder ModelCore
modelCoreDecoder =
    D.map3
        ModelCore
        (D.field "timestamp" D.int)
        (D.field "todos" (D.list todoItemDecoder))
        (D.field "checkpoint" D.int)


encodeModelCore : ModelCore -> E.Value
encodeModelCore modelCore =
    E.object
        [ ( "timestamp", E.int modelCore.timestamp )
        , ( "todos", E.list encodeTodoItem modelCore.todoItems )
        , ( "checkpoint", E.int modelCore.checkpoint )
        ]
