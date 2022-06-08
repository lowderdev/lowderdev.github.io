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
import TileSvg exposing (barSvg, elbowSvg, knobSvg, teeSvg)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initGameState
        , update = update
        , view = view
        }


type Msg
    = Reset
    | RotateTile Coords


update : Msg -> Model -> Model
update msg model =
    case msg of
        Reset ->
            model

        RotateTile coords ->
            Dict.update coords (andThen (\cell -> Just { cell | rotations = cell.rotations + 1 })) model


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
    column [ width (px 800), centerX, centerY, padding 20, Border.width 1, Border.color (rgb 0 0 0.9) ] contents


boardRow : List (Element msg) -> Element msg
boardRow elements =
    row [ centerX, centerY, width (px 600), Border.width 1, Border.color (rgb 0 0.9 0) ] elements


viewCell : Cell -> Coords -> Element Msg
viewCell cell coords =
    el
        [ width (px 60)
        , height (px 60)
        , centerX
        , centerY
        , Border.width 1
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
