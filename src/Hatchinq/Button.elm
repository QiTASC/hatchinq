module Hatchinq.Button exposing (Config, View, configure, contained, outlined, text)

{-|


# Exposed

@docs Config, View, configure, contained, outlined, text

-}

import Element exposing (Element, focused, height, minimum, mouseDown, mouseOver, paddingXY, px, shrink, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Hatchinq.Attribute exposing (Attribute, custom, toElement, toInternalConfig)
import Hatchinq.Theme exposing (Theme)
import Html.Attributes


{-| -}
type alias Config =
    { theme : Theme
    }


{-| -}
type alias View msg =
    { label : String
    , onPress : Maybe msg
    }


type alias InternalConfig msg =
    { label : String
    , onPress : Maybe msg
    , buttonType : ButtonType
    }


type ButtonType
    = Text
    | Outlined
    | Contained


{-| -}
configure : Config -> (List (Attribute (InternalConfig msg)) -> View msg -> Element msg)
configure config =
    view config


{-| -}
contained : Attribute (InternalConfig msg)
contained =
    custom (\v -> { v | buttonType = Contained })


{-| -}
outlined : Attribute (InternalConfig msg)
outlined =
    custom (\v -> { v | buttonType = Outlined })


{-| -}
text : Attribute (InternalConfig msg)
text =
    custom (\v -> { v | buttonType = Text })


view : Config -> List (Attribute (InternalConfig msg)) -> View msg -> Element msg
view { theme } source data =
    let
        defaultView =
            { label = data.label
            , onPress = data.onPress
            , buttonType = Outlined
            }

        internalConfig =
            defaultView |> toInternalConfig source

        disabled =
            internalConfig.onPress == Nothing

        isOutlined =
            internalConfig.buttonType == Outlined

        isFilled =
            internalConfig.buttonType == Contained

        isText =
            internalConfig.buttonType == Text

        attributes =
            toElement source
    in
    Element.el attributes
        (Input.button
            (height (px 36)
                :: width (shrink |> minimum 64)
                :: paddingXY
                    (if isText then
                        8

                     else
                        16
                    )
                    0
                :: Font.family [ theme.font.main ]
                :: Font.size 14
                :: Font.bold
                :: Font.color
                    (if disabled then
                        theme.colors.gray.color

                     else if isFilled then
                        theme.colors.secondary.textColor

                     else
                        theme.colors.secondary.color
                    )
                :: Font.center
                :: Background.color
                    (if isFilled then
                        if disabled then
                            theme.colors.gray.lighter

                        else
                            theme.colors.secondary.color

                     else
                        Element.rgba255 0 0 0 0
                    )
                :: Border.rounded 2
                :: (if not disabled && isFilled then
                        Border.shadow { offset = ( 0, 2 ), size = 0, blur = 3, color = Element.rgba255 140 140 140 0.74 }

                    else
                        Border.width
                            (if isOutlined then
                                1

                             else
                                0
                            )
                   )
                :: Border.color (Element.rgba255 0 0 0 0.12)
                :: Element.htmlAttribute
                    (Html.Attributes.class
                        (if disabled then
                            ""

                         else if isFilled then
                            "button focusWhiteRipple"

                         else
                            "button focusSecondaryRipple"
                        )
                    )
                :: Element.htmlAttribute
                    (if disabled then
                        Html.Attributes.style "cursor" "default"

                     else
                        Html.Attributes.style "" ""
                    )
                :: focused
                    (if disabled then
                        []

                     else if isFilled then
                        [ Background.color theme.colors.secondary.light ]

                     else
                        [ Background.color theme.colors.secondary.lighter ]
                    )
                :: mouseDown
                    (if disabled then
                        []

                     else if isFilled then
                        [ Border.shadow { offset = ( 0, 6 ), size = 0, blur = 10, color = Element.rgba255 140 140 140 0.74 } ]

                     else
                        []
                    )
                :: mouseOver
                    (if disabled then
                        []

                     else if isFilled then
                        [ Background.color (theme.colors.secondary.withAlpha 0.88)
                        , Border.shadow { offset = ( 0, 4 ), size = 0, blur = 6, color = Element.rgba255 140 140 140 0.74 }
                        ]

                     else
                        [ Background.color theme.colors.secondary.lightest ]
                    )
                :: attributes
            )
            { onPress = internalConfig.onPress, label = Element.text internalConfig.label }
        )
