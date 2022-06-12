module Model exposing (..)

import Dict exposing (Dict)
import Maybe exposing (withDefault)
import Random
import Svg.Styled.Attributes exposing (x)


type alias Model =
    Dict Coords Cell


type alias Coords =
    ( Int, Int )


type alias Cell =
    { shape : Shape, rotations : Int }


type Shape
    = Knob
    | Bar
    | Elbow
    | Tee
    | Empty


intToShape : Int -> Shape
intToShape int =
    case int of
        0 ->
            Knob

        1 ->
            Bar

        2 ->
            Elbow

        3 ->
            Tee

        _ ->
            Empty


type alias Board =
    Dict Coords Connections


type alias Connections =
    { n : Bool, w : Bool, s : Bool, e : Bool }


generateConnections : Coords -> Int -> Random.Seed -> Board -> ( Board, Random.Seed )
generateConnections (( x, y ) as coords) maxCoord seed0 board =
    let
        lookN =
            Dict.get ( x - 1, y ) board
                |> Maybe.andThen (\{ s } -> Just s)
                |> withDefault False

        lookW =
            Dict.get ( x, y - 1 ) board
                |> Maybe.andThen (\{ e } -> Just e)
                |> withDefault False

        ( lookS, seed1 ) =
            if x == maxCoord then
                ( False, seed0 )

            else
                Random.step (Random.int 0 1) seed0
                    |> (\( int, seed ) -> ( int == 1, seed ))

        ( lookE, seed2 ) =
            if y == maxCoord then
                ( False, seed1 )

            else
                Random.step (Random.int 0 1) seed1
                    |> (\( int, seed ) -> ( int == 1, seed ))
    in
    ( Dict.insert coords { n = lookN, w = lookW, s = lookS, e = lookE } board, seed2 )


generateBoard : Int -> Random.Seed -> Board
generateBoard boardSize seed0 =
    let
        maxCoord =
            boardSize - 1

        xy =
            List.range 0 maxCoord

        listOfCoords =
            cartesian xy xy
    in
    Tuple.first <|
        List.foldl
            (\coords ( board, seed ) -> generateConnections coords maxCoord seed board)
            ( Dict.empty, seed0 )
            listOfCoords



-- generateCell { boardSize, seed0 } coords =
--     let
--         maxCoord =
--             boardSize - 1
--     in


onCorner : Coords -> Int -> Bool
onCorner coords maxCoord =
    let
        corners =
            [ ( 0, 0 ), ( 0, maxCoord ), ( maxCoord, 0 ), ( maxCoord, maxCoord ) ]
    in
    List.member coords corners


onEdge : Coords -> Int -> Bool
onEdge ( x, y ) maxCoord =
    x == 0 || x == maxCoord || y == 0 || y == maxCoord


initGameState : Int -> Random.Seed -> Model
initGameState boardSize seed0 =
    let
        newBoard =
            generateBoard boardSize seed0

        newModel : Model
        newModel =
            Dict.empty
    in
    Dict.foldl
        (\coords connections model -> Dict.insert coords (toCell connections) model)
        newModel
        newBoard


toCell : Connections -> Cell
toCell { n, w, s, e } =
    if n && not w && not s && not e then
        { shape = Knob, rotations = 0 }

    else if not n && not w && not s && e then
        { shape = Knob, rotations = 1 }

    else if not n && not w && s && not e then
        { shape = Knob, rotations = 2 }

    else if not n && w && not s && not e then
        { shape = Knob, rotations = 3 }

    else if n && not w && not s && e then
        { shape = Elbow, rotations = 0 }

    else if not n && not w && s && e then
        { shape = Elbow, rotations = 1 }

    else if not n && w && s && not e then
        { shape = Elbow, rotations = 2 }

    else if n && w && not s && not e then
        { shape = Elbow, rotations = 3 }

    else if n && not w && s && not e then
        { shape = Bar, rotations = 0 }

    else if not n && w && not s && e then
        { shape = Bar, rotations = 1 }

    else if n && not w && s && e then
        { shape = Tee, rotations = 0 }

    else if not n && w && s && e then
        { shape = Tee, rotations = 1 }

    else if n && w && s && not e then
        { shape = Tee, rotations = 2 }

    else if n && w && not s && e then
        { shape = Tee, rotations = 3 }

    else
        emptyCell


emptyCell : Cell
emptyCell =
    { shape = Empty, rotations = 0 }


cartesian : List a -> List b -> List ( a, b )
cartesian xs ys =
    List.concatMap (\x -> List.map (\y -> ( x, y )) ys) xs
