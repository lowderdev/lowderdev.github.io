module Main exposing (main)

import Browser
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Html exposing (Html)
import Maybe exposing (andThen, withDefault)
import Model exposing (Cell, Coords, Model, Shape(..), initGameState, emptyCell)


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
    case cell.shape of
        Knob ->
            showTile cell
                coords
                [ el [ width (px 15), height (px 15), centerX, centerY, moveRight 15, Background.color (rgb 1 1 1), Border.rounded 10 ] none
                , el [ width (px 30), height (px 6), alignRight, Background.color (rgb 1 1 1) ] none
                ]

        Bar ->
            showTile cell
                coords
                [ el [ width (px 30), height (px 6), alignLeft, Background.color (rgb 1 1 1) ] none
                , el [ width (px 30), height (px 6), alignRight, Background.color (rgb 1 1 1), moveLeft 14 ] none
                ]

        Elbow ->
            showTile cell
                coords
                [ el [ width (px 6), height (px 30), alignBottom, centerX, Background.color (rgb 1 1 1), moveRight 14 ] none
                , el [ width (px 30), height (px 6), alignRight, Background.color (rgb 1 1 1) ] none
                ]

        Tee ->
            showTile cell
                coords
                [ el [ width (px 30), height (px 6), alignLeft, Background.color (rgb 1 1 1) ] none
                , el [ width (px 6), height (px 30), alignBottom, centerX, Background.color (rgb 1 1 1), moveLeft 9 ] none
                , el [ width (px 30), height (px 6), alignRight, Background.color (rgb 1 1 1), moveLeft 14 ] none
                ]

        Empty ->
            showTile cell coords []


showTile : Cell -> Coords -> List (Element Msg) -> Element Msg
showTile cell coords elements =
    row
        [ width (px 60)
        , height (px 60)
        , centerX
        , centerY
        , Border.width 1
        , Border.color (rgb 0.9 0 0)
        , rotate (degrees (toFloat (cell.rotations * 90)))
        , Events.onClick (RotateTile coords)
        ]
        elements
