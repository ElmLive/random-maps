module Main exposing (..)

import Tilesets.Gervais
import Html exposing (Html)
import Html.Attributes
import Map
import Maps.Town
import Random
import Html.App
import Process
import Time
import Task


map : Int -> Map.Map
map seed =
    Random.step Maps.Town.random (Random.initialSeed seed)
        |> fst


type alias Model =
    { map : Map.Map }


type Msg
    = NewMap Map.Map
    | GenerateAnother


main : Program Never
main =
    Html.App.program
        { init =
            ( { map = map 0 }
            , Random.generate NewMap Maps.Town.random
            )
        , subscriptions = \_ -> Sub.none
        , update =
            \msg model ->
                case msg of
                    NewMap newMap ->
                        ( { model | map = newMap }
                        , Process.sleep (800 * Time.millisecond)
                            |> Task.perform
                                (always GenerateAnother)
                                (always GenerateAnother)
                        )

                    GenerateAnother ->
                        ( model
                        , Random.generate NewMap Maps.Town.random
                        )
        , view = \model -> view model.map Tilesets.Gervais.tile
        }


view : (Map.Point -> Map.Tile) -> (String -> Html msg) -> Html msg
view map tileset =
    let
        tile y x =
            ( x, y )
                |> map
                |> tileset

        row y =
            [0..9]
                |> List.map (tile y)
                |> Html.div [ Html.Attributes.style [ ( "line-height", "1px" ) ] ]
    in
        [0..9]
            |> List.map row
            |> Html.div []
