module Sprite exposing (sprite)

import Html exposing (Html)
import Html.Attributes


tileSize : Int
tileSize =
    32


sprite : Int -> Int -> String -> Int -> Int -> Html msg
sprite sw sh image x y =
    croppedImage ( tileSize, tileSize )
        (toFloat sw)
        (toFloat sh)
        ("/" ++ image)
        (toFloat tileSize)
        (toFloat tileSize)
        (-x * tileSize |> toFloat)
        (-y * tileSize |> toFloat)


croppedImage : ( Int, Int ) -> Float -> Float -> String -> Float -> Float -> Float -> Float -> Html msg
croppedImage ( w, h ) sw sh src iw ih ix iy =
    let
        offsetX =
            (ix * toFloat w / iw |> toString) ++ "px"

        offsetY =
            (iy * toFloat h / ih |> toString) ++ "px"

        background =
            "url(" ++ src ++ ") " ++ offsetX ++ " " ++ offsetY

        size =
            toString (sw * toFloat w / iw)
                ++ "px "
                ++ toString (sh * toFloat h / ih)
                ++ "px"
    in
        Html.div
            [ Html.Attributes.style
                [ ( "width", toString w ++ "px" )
                , ( "height", toString h ++ "px" )
                , ( "background", background )
                , ( "background-size", size )
                , ( "display", "inline-block" )
                , ( "line-height", "1px" )
                ]
            ]
            []
