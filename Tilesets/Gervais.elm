module Tilesets.Gervais exposing (tile)

{- A tileset using free images created by David E. Gervais.

   For a history of the creation of the tileset, see [history]. The tile images were downloaded from
   http://pousse.rapiere.free.fr/tome/ and converted from GIF to PNG format.

   [history]: http://groups.google.com/group/rec.games.roguelike.angband/msg/b1002cd6b7518736
-}

import Tilesets.Schematic
import Sprite exposing (sprite)
import Html exposing (Html)


grounds : Int -> Int -> Html msg
grounds =
    sprite 288 608 "Tilesets/Gervais/dg_grounds32.png"


dungeon : Int -> Int -> Html msg
dungeon =
    sprite 288 320 "Tilesets/Gervais/dg_dungeon32.png"


features : Int -> Int -> Html msg
features =
    sprite 288 416 "Tilesets/Gervais/dg_features32.png"


town0 : Int -> Int -> Html msg
town0 =
    sprite 288 128 "Tilesets/Gervais/dg_town032.png"


tile : String -> Html msg
tile name =
    case name of
        "grass" ->
            grounds 0 1

        "bright grass" ->
            grounds 0 1

        "normal grass" ->
            grounds 1 1

        "dim grass" ->
            grounds 2 1

        "dark grass" ->
            grounds 5 2

        "water" ->
            grounds 0 2

        "mountain" ->
            grounds 0 13

        "forest" ->
            grounds 1 6

        "desert" ->
            grounds 3 1

        "bright desert" ->
            grounds 3 1

        "normal desert" ->
            grounds 4 1

        "dim desert" ->
            grounds 5 1

        "dark desert" ->
            grounds 8 4

        "open" ->
            dungeon 2 0

        "wall" ->
            dungeon 0 3

        "ice" ->
            grounds 6 11

        "bright ice" ->
            grounds 3 9

        "normal ice" ->
            grounds 4 9

        "dim ice" ->
            grounds 5 9

        "dark ice" ->
            grounds 8 11

        "desert mountain" ->
            grounds 6 13

        "water1" ->
            grounds 6 1

        "water2" ->
            grounds 0 2

        -- same as "water"
        "barrier" ->
            features 3 6

        "floor" ->
            town0 0 0

        "tree" ->
            grounds 0 6

        "oldtree" ->
            grounds 3 6

        "road" ->
            town0 0 0

        -- Exits
        "cave" ->
            grounds 5 13

        "town" ->
            grounds 0 7

        "boss" ->
            dungeon 8 3

        "outland" ->
            dungeon 6 4

        "armor shop" ->
            features 4 2

        "weapon shop" ->
            features 5 2

        "inn" ->
            features 5 7

        "entrance" ->
            features 3 3

        _ ->
            Tilesets.Schematic.tile name
