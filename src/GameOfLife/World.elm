module GameOfLife.World exposing
    ( Coordinate
    , World
    , generateCoordinates
    , getCells
    , getSize
    , initialize
    , nextGeneration
    , setup
    )

import Dict exposing (Dict)
import GameOfLife.Cell as Cell


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
    { cells : List Cell.Cell
    , coordinates : List Coordinate
    , size : Int
    , cells2 : Dict Coordinate Cell.Cell
    }



-- PUBLIC


setup : Int -> World
setup size =
    { cells = []
    , coordinates = []
    , cells2 = Dict.empty
    , size = size
    }


initialize : List Cell.Cell -> World -> World
initialize cells world =
    { world
        | cells2 =
            List.map2 Tuple.pair (generateCoordinates world) cells
                |> Dict.fromList
    }


getCells : World -> List ( Coordinate, Cell.Cell )
getCells world =
    world.cells2 |> Dict.toList


getSize : World -> Int
getSize world =
    world.size


nextGeneration : World -> World
nextGeneration world =
    getCells world
        |> List.map
            (\( coordinate, cell ) ->
                let
                    mutateCell =
                        cell
                            |> Cell.mutate (neighbours coordinate world)
                in
                ( coordinate, mutateCell )
            )
        |> updateCells world



-- PRIVATE


updateCells : World -> List ( Coordinate, Cell.Cell ) -> World
updateCells world cells =
    { world | cells2 = Dict.fromList cells }



neighbours : Coordinate -> World -> List Cell.Cell
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
                Dict.get ( x - cx, y - cy ) world.cells2 |> Maybe.withDefault Cell.Dead
            )
