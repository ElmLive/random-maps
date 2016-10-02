module Map exposing (any)


type alias Point =
    { x : Int, y : Int }


type alias Tile =
    String


any : (Tile -> Bool) -> (Point -> Tile) -> Bool
any predicate map =
    any' predicate map { x = 0, y = 0 }


any' : (Tile -> Bool) -> (Point -> Tile) -> Point -> Bool
any' predicate map point =
    let
        tile =
            map point
    in
        case predicate tile of
            True ->
                True

            False ->
                if point.y >= 10 then
                    False
                else if point.x >= 10 then
                    any' predicate map { point | x = 0, y = point.y + 1 }
                else
                    any' predicate map { point | x = point.x + 1 }
