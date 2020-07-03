module GameOfLife.Cell exposing (Cell(..), randomCell)

import Random


type Cell
    = Dead
    | Live

randomCell : Random.Generator Cell
randomCell =
    Random.uniform Dead [ Live ]
