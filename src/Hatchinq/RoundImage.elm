module Hatchinq.RoundImage exposing (roundImage)

import Element exposing (Attribute, Element, Length, html)
import Html exposing (img)
import Html.Attributes exposing (alt, height, src, style, width)


roundImage : Int -> Int -> List (Attribute msg) -> String -> Element msg
roundImage widthLength heightLength attributes source =
    Element.el attributes
        (html
            (img
                [ src source
                , alt ""
                , width widthLength
                , height heightLength
                , style "border-radius" "50%"
                , style "object-fit" "cover"
                ]
                []
            )
        )
