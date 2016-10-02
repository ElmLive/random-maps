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


townMapTests : Test
townMapTests =
    describe "Town map generation"
        [ fuzz Fuzz.int "contains at least 10% grass" <|
            \seed ->
                Random.step Maps.Town.random (Random.initialSeed seed)
                    |> fst
                    |> countTiles "grass"
                    |> Expect.greaterThan 10
        , fuzz Fuzz.int "contains at least 10% road" <|
            \seed ->
                Random.step Maps.Town.random (Random.initialSeed seed)
                    |> fst
                    |> countTiles "road"
                    |> Expect.greaterThan 10
        , fuzz Fuzz.int "has exactly one inn" <|
            \seed ->
                Random.step Maps.Town.random (Random.initialSeed seed)
                    |> fst
                    |> countTiles "inn"
                    |> Expect.equal 1
        , fuzz Fuzz.int "has at least one armor shop" <|
            \seed ->
                Random.step Maps.Town.random (Random.initialSeed seed)
                    |> fst
                    |> countTiles "armor shop"
                    |> Expect.greaterThan 0
        , fuzz Fuzz.int "has at least one weapon shop" <|
            \seed ->
                Random.step Maps.Town.random (Random.initialSeed seed)
                    |> fst
                    |> countTiles "weapon shop"
                    |> Expect.greaterThan 0
        ]


countTiles : String -> Map.Map -> Int
countTiles tileToCount map =
    Map.fold
        (\tile acc ->
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
