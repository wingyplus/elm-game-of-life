module World exposing
    ( Cell(..)
    , Coordinate
    , World
    , cellAt
    , generateCoordinates
    , nextGeneration
    , nextGenerationOfCell
    , randomCell
    )

import Array
import Random


type alias Coordinate =
    ( Int, Int )


generateCoordinates : World -> List Coordinate
generateCoordinates world =
    let
        idx =
            List.range 0 (world.size - 1)
    in
    List.concatMap (\x -> List.map (\y -> ( x, y )) idx) idx


type Cell
    = Dead
    | Live


type alias World =
    { cells : List Cell
    , coordinates : List Coordinate
    , size : Int
    }


cellAt : Coordinate -> World -> Maybe Cell
cellAt ( x, y ) world =
    world.cells
        |> Array.fromList
        |> Array.get ((y * world.size) + x)


nextGenerationOfCell : List Cell -> Cell -> Cell
nextGenerationOfCell cellNeighbours cell =
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
                cellAt ( x - cx, y - cy ) world |> Maybe.withDefault Dead
            )


nextGeneration : World -> World
nextGeneration world =
    { world
        | cells =
            List.map2
                (\cell coordinate ->
                    cell
                        |> nextGenerationOfCell (neighbours coordinate world)
                )
                world.cells
                world.coordinates
    }


randomCell : Random.Generator Cell
randomCell =
    Random.uniform Dead [ Live ]
