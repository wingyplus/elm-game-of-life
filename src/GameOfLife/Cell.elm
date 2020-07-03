module GameOfLife.Cell exposing (Cell(..), mutate, randomCell)

import Random


type Cell
    = Dead
    | Live


randomCell : Random.Generator Cell
randomCell =
    Random.uniform Dead [ Live ]


{-| mutate cell depends on neighbour
-}
mutate : List Cell -> Cell -> Cell
mutate cellNeighbours cell =
    let
        liveCells =
            List.filter (\cell_ -> cell_ == Live) cellNeighbours |> List.length
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
