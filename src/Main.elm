module Main exposing (..)

import Tilesets.Gervais
import Html exposing (Html)
import Html.Attributes
import Map
import Maps.Town
import Random


map : Int -> Map.Map
map seed =
    Random.step Maps.Town.random (Random.initialSeed seed)
        |> fst


main : Html msg
main =
    view
        (map 12)
        Tilesets.Gervais.tile


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
