module Main exposing (main)

import Browser
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Html exposing (Html)
import Maybe exposing (andThen)
import Model exposing (Cell, Coords, Model, Shape(..), emptyCell, initGameState, intToShape)
import Random
import TileSvg exposing (barSvg, borderWidth, elbowSvg, knobSvg, teeSvg, tileWidth)


boardSize : Int
boardSize =
    5


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
    ( initGameState boardSize (Random.initialSeed 1), Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = Reset
      -- | GenerateCells
      -- | NewCells (List Cell)
    | RotateTile Coords



-- generateSeed : Cmd Msg
-- generateSeed =
--     Random.generate NewCells (Random.list (boardSize * boardSize) randomCell)


randomCell : Random.Generator Cell
randomCell =
    Random.map2
        (\x y -> { shape = intToShape x, rotations = y })
        (Random.int 0 3)
        (Random.int -3 0)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            ( model, Cmd.none )

        -- GenerateCells ->
        --     ( model, generateCells )
        -- NewCells randomCells ->
        --     ( initGameState randomCells, Cmd.none )
        RotateTile coords ->
            let
                newModel =
                    Dict.update
                        coords
                        (andThen (\cell -> Just { cell | rotations = cell.rotations + 1 }))
                        model
            in
            ( newModel, Cmd.none )


view : Model -> Html Msg
view model =
    let
        widthHeight =
            round <| sqrt <| toFloat <| Dict.size <| model

        tiles =
            Dict.foldr (\k v acc -> viewCell k v :: acc) [] model

        rows =
            split widthHeight tiles
    in
    layout []
        (body
            (gameWindow (List.map (\x -> boardRow x) rows))
        )


split : Int -> List a -> List (List a)
split toTake list =
    case list of
        [] ->
            []

        _ ->
            List.take toTake list :: split toTake (List.drop toTake list)


body : Element msg -> Element msg
body content =
    el [ width fill, height fill, Background.color (rgb255 17 43 60) ] content


gameWindow : List (Element msg) -> Element msg
gameWindow contents =
    column
        [ centerX
        , centerY
        , Border.width borderWidth
        , Border.color
            (rgb255 17 43 60)
        ]
        contents


boardRow : List (Element msg) -> Element msg
boardRow elements =
    row [ centerX, centerY ] elements


viewCell : Coords -> Cell -> Element Msg
viewCell coords cell =
    el
        [ width (px tileWidth)
        , height (px tileWidth)
        , centerX
        , centerY
        , Border.width borderWidth
        , Border.color (rgb255 17 43 60)
        , Border.rounded 6
        , Background.color (rgb255 32 83 117)
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
