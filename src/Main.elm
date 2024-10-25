module Main exposing (..)

import Browser
import Html exposing (Html, div, text)


type alias Model =
    Int


type Msg
    = Nop


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Int -> ( Model, Cmd Msg )
init flag =
    ( flag, Cmd.none )


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
