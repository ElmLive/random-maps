module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz
import String
import Map
import Dict exposing (Dict)
import Maps.Town
import Random


mapTests : Test
mapTests =
    describe "Map module"
        [ test "any with all points false" <|
            \() ->
                Map.any (\tile -> False) (always "grass")
                    |> Expect.false "Expected any to return false"
        , test "any with all points true" <|
            \() ->
                Map.any (\tile -> True) (always "grass")
                    |> Expect.true "Expected any to return true"
        , test "any with first point false, and others true" <|
            \() ->
                Map.any (\tile -> tile == "grass")
                    (\( x, y ) ->
                        if x == 0 then
                            "rock"
                        else
                            "grass"
                    )
                    |> Expect.true "Expected any to return true"
        , test "any with first row false, and others true" <|
            \() ->
                Map.any (\tile -> tile == "grass")
                    (\( x, y ) ->
                        if y == 0 then
                            "rock"
                        else
                            "grass"
                    )
                    |> Expect.true "Expected any to return true"
        ]


createMap : Int -> Map.Map
createMap seed =
    Random.step Maps.Town.random (Random.initialSeed seed)
        |> fst


townMapTests : Test
townMapTests =
    describe "Town map generation"
        [ fuzz Fuzz.int "contains at least 10% grass" <|
            \seed ->
                createMap seed
                    |> countTiles "grass"
                    |> Expect.greaterThan 10
        , fuzz Fuzz.int "contains at least 10% road" <|
            \seed ->
                createMap seed
                    |> countTiles "road"
                    |> Expect.greaterThan 10
        , describe "inn"
            [ fuzz Fuzz.int "has exactly one" <|
                \seed ->
                    createMap seed
                        |> countTiles "inn"
                        |> Expect.equal 1
            , fuzz Fuzz.int "is connected to roads" <|
                \seed ->
                    let
                        map =
                            createMap seed

                        innLocations =
                            Map.findAll "inn" map
                    in
                        innLocations
                            |> List.all (isAdjacentTo map "road")
                            |> Expect.true "all inns to be adjacent to roads"
            ]
        , describe "armor shops"
            [ fuzz Fuzz.int "has at least one armor shop" <|
                \seed ->
                    createMap seed
                        |> countTiles "armor shop"
                        |> Expect.greaterThan 0
            , fuzz Fuzz.int "is connected to roads" <|
                \seed ->
                    let
                        map =
                            createMap seed

                        innLocations =
                            Map.findAll "armor shop" map
                    in
                        innLocations
                            |> List.all (isAdjacentTo map "road")
                            |> Expect.true "all armor shops to be adjacent to roads"
            ]
        , describe "weapon shops"
            [ fuzz Fuzz.int "has at least one weapon shop" <|
                \seed ->
                    createMap seed
                        |> countTiles "weapon shop"
                        |> Expect.greaterThan 0
            , fuzz Fuzz.int "is connected to roads" <|
                \seed ->
                    let
                        map =
                            createMap seed

                        innLocations =
                            Map.findAll "weapon shop" map
                    in
                        innLocations
                            |> List.all (isAdjacentTo map "road")
                            |> Expect.true "all weapon shops to be adjacent to roads"
            ]
        ]


isAdjacentTo : Map.Map -> Map.Tile -> Map.Point -> Bool
isAdjacentTo map expectedTile ( x, y ) =
    (map ( x - 1, y ) == expectedTile)
        || (map ( x + 1, y ) == expectedTile)
        || (map ( x, y - 1 ) == expectedTile)
        || (map ( x, y + 1 ) == expectedTile)


countTiles : String -> Map.Map -> Int
countTiles tileToCount map =
    Map.fold
        (\point tile acc ->
            if tile == tileToCount then
                acc + 1
            else
                acc
        )
        0
        map


all : Test
all =
    describe "all tests"
        [ mapTests
        , townMapTests
        ]
