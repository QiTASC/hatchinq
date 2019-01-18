module Hatchinq.IconButton exposing (Config, View, view)

import Element exposing (Element, centerX, centerY, focused, height, mouseOver, padding, px, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Hatchinq.Attribute exposing (Attribute, toElement)
import Hatchinq.Theme exposing (Theme, icon)
import Html.Attributes



-- CONFIG


type alias Config =
    { theme : Theme
    }



-- VIEW


type alias View msg =
    { icon : String
    , onPress : Maybe msg
    }


view : Config -> List (Attribute v) -> View msg -> Element msg
view { theme } source data =
    let
        disabled =
            data.onPress == Nothing

        attributes =
            toElement source
    in
    Element.el attributes
        (Input.button
            (height (px 48)
                :: width (px 48)
                :: Border.rounded 24
                :: Font.family [ theme.font.main ]
                :: Font.size 24
                :: Font.bold
                :: Font.color
                    (if disabled then
                        theme.colors.gray.color

                     else
                        theme.colors.gray.withAlpha 1
                    )
                :: Font.center
                :: padding 12
                :: Element.htmlAttribute
                    (Html.Attributes.class
                        (if disabled then
                            ""

                         else
                            "ripple focusGrayRipple"
                        )
                    )
                :: focused
                    (if disabled then
                        []

                     else
                        [ Background.color theme.colors.gray.lighter ]
                    )
                :: mouseOver
                    (if disabled then
                        []

                     else
                        [ Background.color theme.colors.gray.lightest ]
                    )
                :: attributes
            )
            { onPress = data.onPress, label = Element.el [ centerX, centerY ] (icon data.icon) }
        )
