module TodoItem exposing (..)

import Json.Decode as D
import Json.Encode as E
import TodoState exposing (..)


type alias TodoItem =
    { title : String
    , modificationTime : Int
    , isEditing : Bool
    , state : TodoState
    , isBlocked : Bool
    }


todoItemDecoder : D.Decoder TodoItem
todoItemDecoder =
    D.map5
        TodoItem
        (D.field "title" D.string)
        (D.field "modified" D.int)
        (D.field "is_editing" D.bool)
        (D.field "state" decodeTodoState)
        (D.field "is_blocked" D.bool)


encodeTodoItem : TodoItem -> E.Value
encodeTodoItem todoItem =
    E.object
        [ ( "title", E.string todoItem.title )
        , ( "modified", E.int todoItem.modificationTime )
        , ( "is_editing", E.bool todoItem.isEditing )
        , ( "state", encodeTodoState todoItem.state )
        , ( "is_blocked", E.bool todoItem.isBlocked )
        ]
