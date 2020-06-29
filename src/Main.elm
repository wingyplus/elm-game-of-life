module Main exposing (main)

import Array exposing (Array)
import Browser
import Random
import Random.Array exposing (array)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time
import World exposing (Cell(..), Coordinate, World, generateCoordinates, nextGeneration, randomCell)


type alias Flags =
    ()


type alias Model =
    { -- generate world size x size.
      size : Int
    , pixelPerCell : Int
    , world : World
    , initialized : Bool
    }


type Msg
    = Tick Time.Posix
    | Generated (Array Cell)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- INIT


{-| NOTE(wingyplus): Microsoft Edge and Chrome crashed when board size
is more than 70. Firefox works fine.
TODO(wingyplus): Remove size.
-}
init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        size =
            60
    in
    ( { size = size
      , pixelPerCell = 32
      , world =
            { cells = []
            , coordinates = []
            , size = size
            }
      , initialized = False
      }
    , array (size * size) randomCell
        |> Random.generate Generated
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Generated cells ->
            let
                world =
                    model.world
            in
            ( { model
                | world =
                    { world
                        | cells = cells |> Array.toList
                        , coordinates = generateCoordinates world
                    }
                , initialized = True
              }
            , Cmd.none
            )

        Tick _ ->
            ( { model
                | world = nextGeneration model.world
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.initialized then
        Time.every 500 Tick

    else
        Sub.none



-- VIEW


view : Model -> Svg Msg
view model =
    let
        worldSize =
            (model.pixelPerCell * model.size) |> String.fromInt
    in
    svg [ width worldSize, height worldSize, viewBox ("0 0 " ++ worldSize ++ " " ++ worldSize) ]
        (drawWorld model.pixelPerCell model.world)


drawWorld : Int -> World -> List (Svg Msg)
drawWorld pixelPerCell world =
    List.map2
        (\cell coordinate ->
            drawCell pixelPerCell coordinate cell
        )
        world.cells
        world.coordinates


drawCell : Int -> Coordinate -> Cell -> Svg Msg
drawCell pixelPerCell ( cx, cy ) cell =
    rect
        [ cx * pixelPerCell |> String.fromInt |> x
        , cy * pixelPerCell |> String.fromInt |> y
        , pixelPerCell |> String.fromInt |> width
        , pixelPerCell |> String.fromInt |> height
        , Svg.Attributes.style (cellStyle cell)
        ]
        []


cellStyle : Cell -> String
cellStyle cell =
    case cell of
        Dead ->
            "fill:rgb(255,255,255)"

        Live ->
            "fill:rgb(0,0,0)"
