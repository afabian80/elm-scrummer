port module Main exposing (..)

import Browser
import File.Download as Download
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode as D
import Json.Encode as E


type alias Model =
    Int


type Msg
    = Increment
    | Download


port saveToLocalStorage : E.Value -> Cmd msg


main : Program E.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = subscriptions
        }


modelDecoder : D.Decoder Int
modelDecoder =
    D.field "age" D.int


encodeModel : Model -> E.Value
encodeModel model =
    E.object [ ( "age", E.int model ) ]


init : E.Value -> ( Model, Cmd Msg )
init flag =
    let
        f =
            D.decodeValue modelDecoder flag
    in
    case f of
        Ok model ->
            ( model, Cmd.none )

        Err _ ->
            ( 0, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ text (String.fromInt model)
        , button [ onClick Increment ] [ text "Increment" ]
        , button [ onClick Download ] [ text "Download" ]
        ]


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
    ( newModel, Cmd.batch [ saveToLocalStorage (encodeModel newModel), cmds ] )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( model + 1, Cmd.none )

        Download ->
            ( model, Download.string "akos.json" "text/json" (E.encode 4 (encodeModel model)) )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
