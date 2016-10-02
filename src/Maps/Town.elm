module Maps.Town exposing (map)

import Dict exposing (Dict)
import Map


map : Map.Point -> Map.Tile
map =
    Dict.empty
        |> Dict.insert ( 5, 5 ) "grass"
        |> Dict.insert ( 5, 4 ) "road"
        |> Map.dictMap "water"
