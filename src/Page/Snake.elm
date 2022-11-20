module Page.Snake exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Events
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Html.Attributes
import List.Extra
import Page.Colors as Colors
import Page.GameBoard exposing (Coords, Shape(..))



-- Model


type alias Model =
    { boardSize : Int
    , grid : Grid
    , snakeCoords : SnakeCoords
    , moveDir : MoveDir
    , moveDelta : Int
    , gameOver : Bool
    }


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


type MoveDir
    = N
    | S
    | E
    | W


type alias Flags =
    { number : Int, windowWidth : Int, windowHeight : Int }



-- Init


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        boardSize =
            5

        mid =
            boardSize // 2

        startingSnakeCoords =
            { head = ( mid, mid ), tail = [ ( mid, mid - 1 ), ( mid, mid - 2 ) ] }

        gridWithSnake =
            buildGrid boardSize startingSnakeCoords
    in
    ( { boardSize = boardSize
      , grid = gridWithSnake
      , snakeCoords = startingSnakeCoords
      , moveDir = E
      , moveDelta = 0
      , gameOver = False
      }
    , Cmd.none
    )


buildGrid : Int -> SnakeCoords -> Grid
buildGrid boardSize snakeCoords =
    let
        snakePoints : List Coords
        snakePoints =
            snakeCoords.head :: snakeCoords.tail

        coordList =
            List.range 0 (boardSize - 1)

        emptyGrid : Dict Coords Cell
        emptyGrid =
            cartesian coordList coordList
                |> List.map (\coord -> ( coord, Empty ))
                |> Dict.fromList

        gridWithSnake =
            List.foldl
                (\coord newGrid -> Dict.insert coord Snake newGrid)
                emptyGrid
                snakePoints
    in
    gridWithSnake



-- Subs


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onAnimationFrameDelta Tick



-- Update


type Msg
    = Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick delta ->
            ( moveSnake delta model, Cmd.none )


moveSnake : Float -> Model -> Model
moveSnake tickDelta ({ boardSize, snakeCoords, moveDir, moveDelta } as model) =
    let
        newMoveDelta =
            moveDelta + floor tickDelta

        { head, tail } =
            snakeCoords

        newHead =
            case moveDir of
                N ->
                    Tuple.mapFirst (\y -> y - 1) head

                S ->
                    Tuple.mapFirst (\y -> y + 1) head

                E ->
                    Tuple.mapSecond (\x -> x + 1) head

                W ->
                    Tuple.mapSecond (\x -> x - 1) head

        newTail =
            head :: Maybe.withDefault [] (List.Extra.init tail)
    in
    if newMoveDelta > 500 then
        { model
            | grid = buildGrid boardSize { head = newHead, tail = newTail }
            , snakeCoords = { head = newHead, tail = newTail }
            , moveDelta = 0
        }

    else
        { model | moveDelta = newMoveDelta }



-- View


view : Model -> Element Msg
view { boardSize, grid } =
    let
        rows =
            List.Extra.groupsOf boardSize (Dict.values grid)
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
        , Border.color Colors.darkBlue
        , Border.rounded 2
        , Background.color Colors.lightBlue
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
