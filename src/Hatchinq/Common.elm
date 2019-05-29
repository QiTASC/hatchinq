module Hatchinq.Common exposing (roundImage)

import Element exposing (Element, centerY, html, paddingXY)
import Html
import Html.Attributes


roundImage : String -> Element msg
roundImage src =
    Element.el [ centerY, paddingXY 16 0 ]
        (html
            (Html.img
                [ Html.Attributes.src src
                , Html.Attributes.alt ""
                , Html.Attributes.width 40
                , Html.Attributes.height 40
                , Html.Attributes.style "border-radius" "50%"
                , Html.Attributes.style "object-fit" "cover"
                ]
                []
            )
        )
