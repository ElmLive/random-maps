module Map exposing (Point, Tile, dictMap, any)

import Dict exposing (Dict)


type alias Point =
    ( Int, Int )


type alias Tile =
    String


type alias Map =
    Point -> Tile



-- Creation


dictMap : Tile -> Dict Point Tile -> Map
dictMap default dict =
    \point ->
        Dict.get point dict
            |> Maybe.withDefault default



-- Inspecting


any : (Tile -> Bool) -> (Point -> Tile) -> Bool
any predicate map =
    any' predicate map ( 0, 0 )


any' : (Tile -> Bool) -> (Point -> Tile) -> Point -> Bool
any' predicate map ( x, y ) =
    let
        tile =
            map ( x, y )
    in
        case predicate tile of
            True ->
                True

            False ->
                if y >= 10 then
                    False
                else if x >= 10 then
                    any' predicate map ( 0, y + 1 )
                else
                    any' predicate map ( x + 1, y )
