module Model exposing (..)

import Dict exposing (Dict)
import List exposing (length)


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


initGameState : List Cell -> Model
initGameState cells =
    let
        xy =
            List.range 0 (length cells)

        listOfCoords =
            cartesian xy xy

        coordsToCell =
            List.map2 Tuple.pair listOfCoords cells
    in
    Dict.fromList coordsToCell


emptyCell : Cell
emptyCell =
    { shape = Empty, rotations = 0 }


cartesian : List a -> List b -> List ( a, b )
cartesian xs ys =
    List.concatMap (\x -> List.map (\y -> ( x, y )) ys) xs
