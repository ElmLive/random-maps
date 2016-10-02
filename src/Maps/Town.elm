module Maps.Town exposing (random, map)

import Dict exposing (Dict)
import Map exposing (Map)
import Random


random : Random.Generator (Map)
random =
    Random.map4 map
        (Random.map3 (,,)
            (Random.pair (Random.int 0 10) (Random.int 1 9))
            (Random.pair (Random.int 0 10) (Random.int 1 9))
            (Random.pair (Random.int 0 10) (Random.int 1 9))
        )
        (Random.pair (Random.int 0 10) (Random.int 1 9))
        (Random.pair (Random.int 0 10) (Random.int 1 9))
        (Random.pair (Random.int 0 10) (Random.int 1 9))


map :
    ( ( Int, Int ), ( Int, Int ), ( Int, Int ) )
    -> ( Int, Int )
    -> ( Int, Int )
    -> ( Int, Int )
    -> Map
map ( ( rx, ry ), ( r2x, r2y ), ( r3x, r3y ) ) innLocation armorLocation weaponLocation =
    let
        addFirstRoad d =
            d
                |> Dict.insert ( rx - 2, ry - 1 ) "road"
                |> Dict.insert ( rx - 1, ry - 1 ) "road"
                |> Dict.insert ( rx, ry - 1 ) "road"
                |> Dict.insert ( rx + 1, ry - 1 ) "road"
                |> Dict.insert ( rx + 2, ry - 1 ) "road"
                |> Dict.insert ( rx + 3, ry - 1 ) "road"

        addSecondRoad d =
            d
                |> Dict.insert ( r2x - 5, r2y - 4 ) "road"
                |> Dict.insert ( r2x - 4, r2y - 4 ) "road"
                |> Dict.insert ( r2x - 3, r2y - 4 ) "road"
                |> Dict.insert ( r2x - 2, r2y - 4 ) "road"
                |> Dict.insert ( r2x - 1, r2y - 4 ) "road"
                |> Dict.insert ( r2x, r2y - 4 ) "road"
                |> Dict.insert ( r2x + 1, r2y - 4 ) "road"
                |> Dict.insert ( r2x + 2, r2y - 4 ) "road"
                |> Dict.insert ( r2x + 3, r2y - 4 ) "road"

        addThridRoad d =
            d
                |> Dict.insert ( r3x, r3y - 6 ) "road"
                |> Dict.insert ( r3x, r3y - 5 ) "road"
                |> Dict.insert ( r3x, r3y - 4 ) "road"
                |> Dict.insert ( r3x, r3y - 3 ) "road"
                |> Dict.insert ( r3x, r3y - 2 ) "road"
                |> Dict.insert ( r3x, r3y - 1 ) "road"
                |> Dict.insert ( r3x, r3y + 0 ) "road"
                |> Dict.insert ( r3x, r3y + 1 ) "road"
                |> Dict.insert ( r3x, r3y + 2 ) "road"
                |> Dict.insert ( r3x, r3y + 3 ) "road"
                |> Dict.insert ( r3x, r3y + 4 ) "road"
                |> Dict.insert ( r3x, r3y + 5 ) "road"
                |> Dict.insert ( r3x, r3y + 6 ) "road"

        moveRoad ( x, y ) d =
            if Dict.get ( x, y ) d == Just "road" then
                d
                    |> Dict.insert ( x - 1, y - 1 ) "road"
                    |> Dict.insert ( x - 1, y + 0 ) "road"
                    |> Dict.insert ( x - 1, y + 1 ) "road"
            else
                d

        connectToRoads ( x, y ) d =
            let
                nextPoint =
                    ( x - 1, y )
            in
                if Dict.get nextPoint d == Nothing then
                    d
                        |> Dict.insert nextPoint "road"
                else
                    d

        placeBuilding ( x, y ) tile d =
            if
                (Dict.get ( x, y ) d == Nothing)
                    && (Dict.get ( x - 1, y ) d == Nothing)
            then
                d
                    |> moveRoad ( x, y )
                    |> Dict.insert ( x, y ) tile
                    |> connectToRoads ( x, y )
            else
                placeBuilding ( (x + 2) % 10, (y + 1) % 10 ) tile d
    in
        Dict.empty
            |> addFirstRoad
            |> addSecondRoad
            |> addThridRoad
            |> placeBuilding innLocation "inn"
            |> placeBuilding armorLocation "armor shop"
            |> placeBuilding (weaponLocation) "weapon shop"
            |> Map.dictMap "grass"
