port module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E


type alias Model =
    Int


type Msg
    = Increment


port saveToLocalStorage : String -> Cmd msg


main : Program String Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = subscriptions
        }


flagDecoder : D.Decoder Int
flagDecoder =
    D.field "age" D.int


encode : Model -> E.Value
encode model =
    E.object [ ( "age", E.int model ) ]


init : String -> ( Model, Cmd Msg )
init flag =
    let
        f =
            D.decodeString flagDecoder flag
    in
    case f of
        Ok x ->
            ( x, Cmd.none )

        Err _ ->
            ( 0, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ text (String.fromInt model)
        , button [ onClick Increment ] [ text "Increment" ]

        -- , button [ onClick saveToLocalStorage ] [ text "Save" ]
        ]


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
    -- ( newModel, Cmd.batch [ saveToLocalStorage (encode model), cmds ] )
    ( newModel, Cmd.batch [ saveToLocalStorage "{\"age\": 23}", cmds ] )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( model + 1, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
