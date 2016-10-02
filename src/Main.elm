module Main exposing (..)

import Tilesets.Gervais
import Html exposing (Html)
import Html.Attributes
import Map
import Maps.Town


main : Html msg
main =
    view
        Maps.Town.map
        Tilesets.Gervais.tile


view : (Map.Point -> Map.Tile) -> (String -> Html msg) -> Html msg
view map tileset =
    let
        tile y x =
            ( x, y )
                |> map
                |> tileset

        row y =
            [0..10]
                |> List.map (tile y)
                |> Html.div [ Html.Attributes.style [ ( "line-height", "1px" ) ] ]
    in
        [0..10]
            |> List.map row
            |> Html.div []
