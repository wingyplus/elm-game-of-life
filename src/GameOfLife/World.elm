module GameOfLife.World exposing
    ( Coordinate
    , World
    , generateCoordinates
    , getCells
    , initialize
    , nextGeneration
    , setup
    )

import Dict exposing (Dict)
import GameOfLife.Cell exposing (Cell(..))


type alias Coordinate =
    ( Int, Int )


generateCoordinates : World -> List Coordinate
generateCoordinates world =
    let
        idx =
            List.range 0 (world.size - 1)
    in
    List.concatMap (\x -> List.map (\y -> ( x, y )) idx) idx


type alias World =
    { cells : List Cell
    , coordinates : List Coordinate
    , size : Int
    , cells2 : Dict Coordinate Cell
    }

-- PUBLIC

setup : Int -> World
setup size =
    { cells = []
    , coordinates = []
    , cells2 = Dict.empty
    , size = size
    }


initialize : List Cell -> World -> World
initialize cells world =
    { world
        | cells2 =
            List.map2 Tuple.pair (generateCoordinates world) cells
                |> Dict.fromList
    }


getCells : World -> List ( Coordinate, Cell )
getCells world =
    world.cells2 |> Dict.toList


nextGeneration : World -> World
nextGeneration world =
    getCells world
        |> List.map
            (\( coordinate, cell ) ->
                let
                    mutateCell =
                        cell
                            |> mutate (neighbours coordinate world)
                in
                ( coordinate, mutateCell )
            )
        |> updateCells world



-- PRIVATE


updateCells : World -> List ( Coordinate, Cell ) -> World
updateCells world cells =
    { world | cells2 = Dict.fromList cells }


{-| mutate cell depends on neighbour
-}
mutate : List Cell -> Cell -> Cell
mutate cellNeighbours cell =
    let
        liveCells =
            List.filter (\ncell -> ncell == Live) cellNeighbours |> List.length
    in
    case cell of
        Dead ->
            if liveCells == 3 then
                Live

            else
                Dead

        Live ->
            if liveCells < 2 then
                Dead

            else if liveCells == 2 || liveCells == 3 then
                Live

            else
                Dead


neighbours : Coordinate -> World -> List Cell
neighbours ( x, y ) world =
    [ ( -1, -1 )
    , ( 0, -1 )
    , ( 1, -1 )
    , ( -1, 0 )
    , ( 1, 0 )
    , ( -1, 1 )
    , ( 0, 1 )
    , ( 1, 1 )
    ]
        |> List.map
            (\( cx, cy ) ->
                Dict.get ( x - cx, y - cy ) world.cells2 |> Maybe.withDefault Dead
            )
