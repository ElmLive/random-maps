module Tilesets.Schematic exposing (tile)

import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes


tile : String -> Html msg
tile name =
    text ( 32, 32 )
        { size = 8, color = Color.darkCharcoal }
        name


text : ( Int, Int ) -> { size : Int, color : Color } -> String -> Html msg
text ( w, h ) style s =
    Html.div
        [ Html.Attributes.style
            [ ( "width", toString w ++ "px" )
            , ( "height", toString h ++ "px" )
            , ( "overflow", "hidden" )
            , ( "font-size", (toString style.size) ++ "px" )
            , ( "color", colorToString style.color )
            ]
        ]
        [ Html.text s ]


colorToString : Color -> String
colorToString c =
    let
        { red, green, blue, alpha } =
            Color.toRgb c
    in
        "rgb(" ++ (toString red) ++ ", " ++ (toString green) ++ ", " ++ (toString blue) ++ ")"
