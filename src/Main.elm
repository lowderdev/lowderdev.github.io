module Main exposing (main)

import Browser
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Html exposing (Html)
import Maybe exposing (andThen, withDefault)
import Model exposing (Cell, Coords, Model, Shape(..), emptyCell, initGameState)
import Random
import TileSvg exposing (barSvg, borderWidth, elbowSvg, knobSvg, teeSvg, tileWidth)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initGameState [ ( 0, 0 ), ( 0, 0 ), ( 0, 0 ), ( 0, 0 ) ], generateRandomNumber )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = Reset
    | GenerateRandomNumber
    | NewRandomNumbers (List ( Int, Int ))
    | RotateTile Coords


zeroToThree : Cmd Msg
zeroToThree =
    Random.generate NewRandomNumbers (Random.list 4 (Random.pair (Random.int 0 3) (Random.int -3 0)))


generateRandomNumber : Cmd Msg
generateRandomNumber =
    zeroToThree


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( model, Cmd.none )

        GenerateRandomNumber ->
            ( model, generateRandomNumber )

        NewRandomNumbers newNums ->
            ( initGameState newNums, Cmd.none )

        RotateTile coords ->
            let
                newModel =
                    Dict.update coords (andThen (\cell -> Just { cell | rotations = cell.rotations + 1 })) model
            in
            ( newModel, Cmd.none )


view : Model -> Html Msg
view model =
    layout [] (viewBoard model)


viewBoard : Model -> Element Msg
viewBoard grid =
    let
        a =
            withDefault emptyCell (Dict.get ( 0, 0 ) grid)

        b =
            withDefault emptyCell (Dict.get ( 0, 1 ) grid)

        c =
            withDefault emptyCell (Dict.get ( 1, 0 ) grid)

        d =
            withDefault emptyCell (Dict.get ( 1, 1 ) grid)
    in
    body
        (gameWindow
            [ boardRow [ viewCell a ( 0, 0 ), viewCell b ( 0, 1 ) ]
            , boardRow [ viewCell c ( 1, 0 ), viewCell d ( 1, 1 ) ]
            ]
        )


body : Element msg -> Element msg
body content =
    el [ width fill, height fill, Background.color (rgb 0.3 0.3 0.3) ] content


gameWindow : List (Element msg) -> Element msg
gameWindow contents =
    column [ centerX, centerY, Border.width borderWidth, Border.color (rgb 0.9 0 0) ] contents


boardRow : List (Element msg) -> Element msg
boardRow elements =
    row [ centerX, centerY ] elements


viewCell : Cell -> Coords -> Element Msg
viewCell cell coords =
    el
        [ width (px tileWidth)
        , height (px tileWidth)
        , centerX
        , centerY
        , Border.width borderWidth
        , Border.color (rgb 0.9 0 0)
        , rotate (degrees (toFloat (cell.rotations * 90)))
        , Events.onClick (RotateTile coords)
        ]
        (case cell.shape of
            Knob ->
                knobSvg

            Bar ->
                barSvg

            Elbow ->
                elbowSvg

            Tee ->
                teeSvg

            Empty ->
                Element.none
        )
