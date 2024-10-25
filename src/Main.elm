module Main exposing (..)

import Browser
import Html exposing (Html, div, text)


type alias Model =
    Int


type Msg
    = Nop


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
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
