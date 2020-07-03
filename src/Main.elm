module Main exposing (main)

import Array exposing (Array)
import Browser
import GameOfLife.Cell exposing (Cell(..), randomCell)
import GameOfLife.World as World
import Random
import Random.Array exposing (array)
import Svg
import Svg.Attributes
import Time


type alias Flags =
    { size : Int
    }


type alias Model =
    { pixelPerCell : Int
    , world : World.World
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
init { size } =
    ( { pixelPerCell = 32
      , world =
            World.setup size
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
            ( { model
                | world =
                    World.initialize (Array.toList cells) model.world
                , initialized = True
              }
            , Cmd.none
            )

        Tick _ ->
            ( { model
                | world = World.nextGeneration model.world
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


view : Model -> Svg.Svg Msg
view model =
    let
        worldSize =
            (model.pixelPerCell * World.getSize model.world) |> String.fromInt
    in
    Svg.svg
        [ Svg.Attributes.width worldSize
        , Svg.Attributes.height worldSize
        , Svg.Attributes.viewBox ("0 0 " ++ worldSize ++ " " ++ worldSize)
        ]
        (renderWorld model.pixelPerCell model.world)


renderWorld : Int -> World.World -> List (Svg.Svg Msg)
renderWorld pixelPerCell world =
    World.getCells world
        |> List.map
            (\( coordinate, cell ) ->
                renderCell pixelPerCell coordinate cell
            )


renderCell : Int -> World.Coordinate -> Cell -> Svg.Svg Msg
renderCell pixelPerCell ( cx, cy ) cell =
    Svg.rect
        [ cx * pixelPerCell |> String.fromInt |> Svg.Attributes.x
        , cy * pixelPerCell |> String.fromInt |> Svg.Attributes.y
        , pixelPerCell |> String.fromInt |> Svg.Attributes.width
        , pixelPerCell |> String.fromInt |> Svg.Attributes.height
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
