module Page.Snake exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Events
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Html.Attributes
import Json.Decode as Decode
import List.Extra
import Page.Colors as Colors
import Random exposing (Seed)



-- Model


type alias Model =
    { boardSize : Int
    , grid : Grid
    , snakeCoords : SnakeCoords
    , moveDir : Direction
    , moveDelta : Int
    , gameOver : Bool
    , seed : Seed
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


type Key
    = Arrow Direction
    | Other


type Direction
    = Left
    | Right
    | Up
    | Down


type alias Flags =
    { number : Int, windowWidth : Int, windowHeight : Int }



-- Init


init : Flags -> ( Model, Cmd Msg )
init { number } =
    let
        boardSize =
            20

        mid =
            boardSize // 2

        seed0 =
            Random.initialSeed number

        startingSnakeCoords =
            { head = ( mid, mid ), tail = [ ( mid, mid - 1 ), ( mid, mid - 2 ) ] }

        ( gridWithEntities, seed1 ) =
            buildGrid boardSize startingSnakeCoords seed0
    in
    ( { boardSize = boardSize
      , grid = gridWithEntities
      , snakeCoords = startingSnakeCoords
      , moveDir = Right
      , moveDelta = 0
      , gameOver = False
      , seed = seed1
      }
    , Cmd.none
    )


buildGrid : Int -> SnakeCoords -> Random.Seed -> ( Grid, Random.Seed )
buildGrid boardSize snakeCoords seed0 =
    let
        snakePoints =
            snakeCoords.head :: snakeCoords.tail

        coordRange =
            List.range 0 (boardSize - 1)

        coordList =
            cartesianProduct coordRange coordRange

        emptyCellCoords =
            List.filter
                (\coord -> not (List.member coord snakePoints))
                coordList

        foodCoord =
            emptyCellCoords
            |> \list -> (List.length list) - 1
            |> \maxIndex -> (Random.step (Random.int 0 maxIndex) seed0)
            |> \(i, seed1) -> 


        emptyGrid : Dict Coords Cell
        emptyGrid =
            coordList
                |> List.map (\coord -> ( coord, Empty ))
                |> Dict.fromList

        gridWithSnake =
            List.foldl
                (\coord newGrid -> Dict.insert coord Snake newGrid)
                emptyGrid
                snakePoints
    in
    ( gridWithSnake, seed0 )



-- Subs


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        , Browser.Events.onKeyDown keyDecoder
        ]



-- Update


type Msg
    = Tick Float
    | KeyDown Key


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick delta ->
            ( moveSnake delta model, Cmd.none )

        KeyDown key ->
            case key of
                Arrow direction ->
                    ( updateMoveDir direction model, Cmd.none )

                Other ->
                    ( model, Cmd.none )


moveSnake : Float -> Model -> Model
moveSnake tickDelta ({ boardSize, snakeCoords, moveDir, moveDelta } as model) =
    let
        newMoveDelta =
            moveDelta + floor tickDelta

        { head, tail } =
            snakeCoords

        newHead =
            case moveDir of
                Up ->
                    Tuple.mapFirst (\y -> y - 1) head

                Down ->
                    Tuple.mapFirst (\y -> y + 1) head

                Right ->
                    Tuple.mapSecond (\x -> x + 1) head

                Left ->
                    Tuple.mapSecond (\x -> x - 1) head

        newTail =
            head :: Maybe.withDefault [] (List.Extra.init tail)

        crash =
            Tuple.first newHead
                >= boardSize
                || Tuple.first newHead
                < 0
                || Tuple.second newHead
                >= boardSize
                || Tuple.second newHead
                < 0
    in
    if newMoveDelta > 100 then
        if crash then
            { model | gameOver = True }

        else
            { model
                | grid = buildGrid boardSize { head = newHead, tail = newTail }
                , snakeCoords = { head = newHead, tail = newTail }
                , moveDelta = 0
            }

    else
        { model | moveDelta = newMoveDelta }


updateMoveDir : Direction -> Model -> Model
updateMoveDir direction model =
    { model | moveDir = direction }



-- View


view : Model -> Element Msg
view { boardSize, grid } =
    let
        rows =
            List.Extra.groupsOf boardSize (Dict.values grid)
    in
    column
        [ centerX, centerY ]
        (List.map viewRow rows)


viewRow : List Cell -> Element Msg
viewRow cellRow =
    row [] (List.map viewCell cellRow)


viewCell : Cell -> Element Msg
viewCell cell =
    let
        color =
            case cell of
                Empty ->
                    Colors.lightBlue

                Snake ->
                    Colors.white

                Food ->
                    Colors.lightBlue
    in
    el
        [ centerX
        , centerY
        , width (px 20)
        , height (px 20)
        , Border.width 1
        , Border.color Colors.darkBlue
        , Border.rounded 2
        , Background.color color
        , htmlAttribute (Html.Attributes.style "touch-action" "manipulation")
        ]
        none


cartesianProduct : List a -> List b -> List ( a, b )
cartesianProduct xs ys =
    List.Extra.lift2 Tuple.pair xs ys


keyDecoder : Decode.Decoder Msg
keyDecoder =
    let
        stringDecoder =
            Decode.field "key" Decode.string

        directionDecode =
            Decode.map toDirection stringDecoder

        msgDecoder =
            Decode.map KeyDown directionDecode
    in
    msgDecoder


toDirection : String -> Key
toDirection string =
    case string of
        "ArrowLeft" ->
            Arrow Left

        "a" ->
            Arrow Left

        "ArrowRight" ->
            Arrow Right

        "d" ->
            Arrow Right

        "ArrowUp" ->
            Arrow Up

        "w" ->
            Arrow Up

        "ArrowDown" ->
            Arrow Down

        "s" ->
            Arrow Down

        _ ->
            Other
