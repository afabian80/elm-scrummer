module Main exposing (..)

import Browser
import Html exposing (Html, div, text)
import Json.Decode as D


type alias Model =
    Int


type Msg
    = Nop


main : Program String Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


flagDecoder : D.Decoder Int
flagDecoder =
    D.field "age" D.int


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
    div [] [ text (String.fromInt model) ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nop ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
