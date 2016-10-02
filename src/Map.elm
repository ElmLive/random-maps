module Map exposing (Map, Point, Tile, dictMap, fold, any, findAll)

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


fold : (Point -> Tile -> a -> a) -> a -> Map -> a
fold fn init map =
    fold' fn init map ( 0, 0 )


fold' : (Point -> Tile -> a -> a) -> a -> Map -> Point -> a
fold' fn init map ( x, y ) =
    let
        tile =
            map ( x, y )

        newValue =
            fn ( x, y ) tile init
    in
        if y >= 10 then
            newValue
        else if x >= 10 then
            fold' fn newValue map ( 0, y + 1 )
        else
            fold' fn newValue map ( x + 1, y )


findAll : Tile -> Map -> List Point
findAll tileToFind map =
    fold
        (\point tile results ->
            if tile == tileToFind then
                point :: results
            else
                results
        )
        []
        map


any : (Tile -> Bool) -> Map -> Bool
any predicate map =
    any' predicate map ( 0, 0 )


any' : (Tile -> Bool) -> Map -> Point -> Bool
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
