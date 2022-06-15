module Model exposing (..)

import Dict exposing (Dict)
import Maybe
import Random


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


type alias Board =
    Dict Coords Connections


type alias Connections =
    { n : Bool, w : Bool, s : Bool, e : Bool }


type Direction
    = N
    | W
    | S
    | E


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


emptyCon : Connections
emptyCon =
    { n = False, w = False, s = False, e = False }


initGameState : Int -> Random.Seed -> Model
initGameState boardSize seed0 =
    let
        maxCoord =
            boardSize - 1

        ( randomCoords, seed1 ) =
            Random.step
                (Random.pair (Random.int 0 maxCoord) (Random.int 0 maxCoord))
                seed0

        initBoard =
            Dict.insert randomCoords emptyCon Dict.empty

        finishedBoard =
            Tuple.first <| depthFirstMazeGen randomCoords [ randomCoords ] maxCoord seed1 initBoard
    in
    Dict.foldl
        (\coords connections model -> Dict.insert coords (toCell connections) model)
        Dict.empty
        finishedBoard


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


depthFirstMazeGen : Coords -> List Coords -> Int -> Random.Seed -> Board -> ( Board, Random.Seed )
depthFirstMazeGen (( x, y ) as currentCoords) searchPoints maxCoord seed0 board =
    let
        canVisitN =
            if x /= 0 && not (Dict.member ( x - 1, y ) board) then
                Just N

            else
                Nothing

        canVisitW =
            if y /= 0 && not (Dict.member ( x, y - 1 ) board) then
                Just W

            else
                Nothing

        canVisitS =
            if x /= maxCoord && not (Dict.member ( x + 1, y ) board) then
                Just S

            else
                Nothing

        canVisitE =
            if y /= maxCoord && not (Dict.member ( x, y + 1 ) board) then
                Just E

            else
                Nothing

        canVisit =
            List.foldr
                (\maybeDirection acc ->
                    case maybeDirection of
                        Just dir ->
                            dir :: acc

                        Nothing ->
                            acc
                )
                []
                [ canVisitN
                , canVisitW
                , canVisitS
                , canVisitE
                ]

        numberOfVisitable =
            List.length canVisit

        allNeighborsVisited =
            numberOfVisitable == 0
    in
    case searchPoints of
        [] ->
            ( board, seed0 )

        (head :: tail) as points ->
            if allNeighborsVisited then
                depthFirstMazeGen head tail maxCoord seed0 board

            else
                let
                    newPoints =
                        currentCoords :: points

                    ( randomInt, seed1 ) =
                        Random.step (Random.int 0 (numberOfVisitable - 1)) seed0

                    neighborDir =
                        case List.drop randomInt canVisit of
                            [] ->
                                N

                            dir :: _ ->
                                dir

                    neighborCoords =
                        case neighborDir of
                            N ->
                                ( x - 1, y )

                            W ->
                                ( x, y - 1 )

                            S ->
                                ( x + 1, y )

                            E ->
                                ( x, y + 1 )

                    newBoard =
                        connectPoints currentCoords neighborCoords neighborDir board
                in
                depthFirstMazeGen neighborCoords newPoints maxCoord seed1 newBoard


connectPoints : Coords -> Coords -> Direction -> Board -> Board
connectPoints currentCoords neighborCoords neighborDir board =
    let
        newBoard1 =
            Dict.update
                currentCoords
                (Maybe.andThen
                    (\cons ->
                        Just <|
                            case neighborDir of
                                N ->
                                    { cons | n = True }

                                W ->
                                    { cons | w = True }

                                S ->
                                    { cons | s = True }

                                E ->
                                    { cons | e = True }
                    )
                )
                board

        newBoard2 =
            Dict.insert
                neighborCoords
                (case neighborDir of
                    N ->
                        { emptyCon | s = True }

                    W ->
                        { emptyCon | e = True }

                    S ->
                        { emptyCon | n = True }

                    E ->
                        { emptyCon | w = True }
                )
                newBoard1
    in
    newBoard2
