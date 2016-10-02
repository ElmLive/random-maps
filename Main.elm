module Main exposing (..)

import Tilesets.Gervais
import Html exposing (Html)
import Html.Attributes


main : Html msg
main =
    view
        (\{ x, y } ->
            case x + y of
                6 ->
                    { tile = "desert" }

                7 ->
                    { tile = "dark desert" }

                _ ->
                    { tile = "grass" }
        )
        Tilesets.Gervais.tile


type alias Point =
    { x : Int, y : Int }


type alias Tile =
    { tile : String }


view : (Point -> Tile) -> (String -> Html msg) -> Html msg
view map tileset =
    let
        tile y x =
            { x = x, y = y }
                |> map
                |> .tile
                |> tileset

        row y =
            [0..10]
                |> List.map (tile y)
                |> Html.div [ Html.Attributes.style [ ( "line-height", "1px" ) ] ]
    in
        [0..10]
            |> List.map row
            |> Html.div []
