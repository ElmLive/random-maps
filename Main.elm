module Main exposing (..)

import Tilesets.Gervais
import Html exposing (Html)
import Html.Attributes


main =
    view
        (\{ x, y } ->
            case x - y - y of
                4 ->
                    { tile = "water" }

                _ ->
                    { tile = "grass" }
        )
        Tilesets.Gervais.tile


type alias Point =
    { x : Int, y : Int }


type alias Tile =
    { tile : String }


view tileFn tileset =
    let
        tile y x =
            { x = x, y = y }
                |> tileFn
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
