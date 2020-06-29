module World exposing (Cell(..), World, cellAt, nextGeneration, nextGenerationOfCell, randomCell)

import Array exposing (Array)
import Random


type Cell
    = Dead
    | Live


type alias World =
    Array Cell


cellAt : Int -> Int -> Int -> World -> Maybe Cell
cellAt px py size world =
    Array.get ((py * size) + px) world


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


neighbours : Int -> Int -> Int -> World -> List Cell
neighbours cellPx cellPy size world =
    [ [ ( -1, -1 ), ( 0, -1 ), ( 1, -1 ) ]
    , [ ( -1, 0 ), ( 1, 0 ) ]
    , [ ( -1, 1 ), ( 0, 1 ), ( 1, 1 ) ]
    ]
        |> List.concat
        |> List.map
            (\( cx, cy ) ->
                cellAt (cellPx - cx) (cellPy - cy) size world
                    |> Maybe.withDefault Dead
            )


nextGeneration : Int -> World -> World
nextGeneration size world =
    List.range 0 (size - 1)
        |> List.map
            (\py ->
                List.range 0 (size - 1)
                    |> List.map
                        (\px ->
                            cellAt px py size world
                                -- TODO(wingyplus): eliminate Maybe.
                                |> Maybe.withDefault Dead
                                |> nextGenerationOfCell (neighbours px py size world)
                        )
            )
        |> List.concat
        |> Array.fromList


randomCell : Random.Generator Cell
randomCell =
    Random.uniform Dead [ Live ]
