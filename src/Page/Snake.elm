module Page.Snake exposing (Model, Msg, init, update, view)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Html.Attributes
import List.Extra exposing (cartesianProduct, groupsOf, lift2)
import Page.Colors exposing (darkBlue, lightBlue)
import Page.GameBoard exposing (Coords, Shape(..))



-- Model


type alias Model =
    { boardSize : Int, grid : Grid, snakeCoords : SnakeCoords, gameOver : Bool }


type alias Grid =
    Dict Coords Cell


type alias Coords =
    ( Int, Int )


type Cell
    = Empty
    | Snake
    | Food


type alias SnakeCoords =
    { head : Coords, tail : List Coords }


type alias Flags =
    { number : Int, windowWidth : Int, windowHeight : Int }


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        boardSize =
            5

        coordList =
            List.range 0 (boardSize - 1)

        emptyGrid : Dict Coords Cell
        emptyGrid =
            cartesian coordList coordList
                |> List.map (\coord -> ( coord, Empty ))
                |> Dict.fromList

        mid =
            boardSize // 2

        startingSnakeCoords =
            { head = ( mid, mid ), tail = [ ( mid, mid - 1 ), ( mid, mid - 2 ) ] }

        snakePoints : List Coords
        snakePoints =
            startingSnakeCoords.head :: startingSnakeCoords.tail

        gridWithSnake =
            List.foldl
                (\coord newGrid -> Dict.insert coord Snake newGrid)
                emptyGrid
                snakePoints
    in
    ( { boardSize = boardSize
      , grid = gridWithSnake
      , snakeCoords = startingSnakeCoords
      , gameOver = False
      }
    , Cmd.none
    )



-- Update


type Msg
    = Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )



-- View


view : Model -> Element Msg
view { boardSize, grid } =
    let
        rows =
            groupsOf boardSize (Dict.values grid)
    in
    column
        [ centerX
        , centerY
        ]
        (List.map viewRow rows)


viewRow : List Cell -> Element Msg
viewRow cellRow =
    row [] (List.map viewCell cellRow)


viewCell : Cell -> Element Msg
viewCell cell =
    el
        [ centerX
        , centerY
        , width (px 80)
        , height (px 80)
        , Border.width 1
        , Border.color darkBlue
        , Border.rounded 2
        , Background.color lightBlue
        , htmlAttribute (Html.Attributes.style "touch-action" "manipulation")
        ]
        (case cell of
            Empty ->
                none

            Snake ->
                text "s"

            Food ->
                text "f"
        )


cartesian : List a -> List b -> List ( a, b )
cartesian xs ys =
    List.Extra.lift2 Tuple.pair xs ys
