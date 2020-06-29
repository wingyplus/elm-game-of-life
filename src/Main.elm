module Main exposing (main)

import Array
import Browser
import Random
import Random.Array exposing (array)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time
import World exposing (Cell(..), World, cellAt, nextGeneration, randomCell)


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
    | Generated World


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- INIT


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        size =
            70
    in
    ( { size = size
      , pixelPerCell = 32
      , world = Array.fromList []
      , initialized = False
      }
    , array (size * size) randomCell
        |> Random.generate Generated
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Generated world ->
            ( { model | world = world, initialized = True }, Cmd.none )

        Tick _ ->
            ( { model
                | world = nextGeneration model.size model.world
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
        (drawWorld model.pixelPerCell model.size model.world)


drawWorld : Int -> Int -> World -> List (Svg Msg)
drawWorld pixelPerCell size world =
    List.range 0 (size - 1)
        |> List.map
            (\py ->
                List.range 0 (size - 1)
                    |> List.map
                        (\px ->
                            -- TODO(wingyplus): eliminate Maybe.
                            cellAt px py size world |> Maybe.withDefault Dead |> drawCell px py pixelPerCell
                        )
            )
        |> List.concat


drawCell : Int -> Int -> Int -> Cell -> Svg Msg
drawCell px py pixelPerCell cell =
    rect
        [ px * pixelPerCell |> String.fromInt |> x
        , py * pixelPerCell |> String.fromInt |> y
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
