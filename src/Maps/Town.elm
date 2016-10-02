module Maps.Town exposing (random, map)

import Dict exposing (Dict)
import Map exposing (Map)
import Random


random : Random.Generator (Map)
random =
    Random.map map (Random.pair (Random.int 0 10) (Random.int 1 9))


map : ( Int, Int ) -> Map
map ( rx, ry ) =
    Dict.empty
        |> Dict.insert ( rx, ry ) "grass"
        |> Dict.insert ( rx, ry - 1 ) "road"
        |> Map.dictMap "water"
