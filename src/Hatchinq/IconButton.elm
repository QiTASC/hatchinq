module Hatchinq.IconButton exposing (Config, View, configure, filled, withTextColor)

{-|


# Exposed

@docs Config, View, configure, filled, withTextColor

-}

import Element exposing (Color, Element, centerX, centerY, focused, height, mouseOver, padding, px, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Hatchinq.Attribute exposing (Attribute, custom, toElement, toInternalView)
import Hatchinq.Color as Color
import Hatchinq.Theme exposing (Theme, black, icon)
import Html.Attributes



-- CONFIG


type IconButtonType
    = Default
    | Filled


type alias InternalConfig =
    { iconButtonType : IconButtonType
    , textColor : Maybe Color
    }


{-| -}
type alias Config =
    { theme : Theme
    }


{-| -}
configure : Config -> (List (Attribute InternalConfig) -> View msg -> Element msg)
configure config =
    view config



-- VIEW


{-| -}
type alias View msg =
    { icon : String
    , onPress : Maybe msg
    }


{-| -}
filled : Attribute InternalConfig
filled =
    custom (\v -> { v | iconButtonType = Filled })


{-| -}
withTextColor : Color -> Attribute InternalConfig
withTextColor color =
    custom (\v -> { v | textColor = Just color })


view : Config -> List (Attribute InternalConfig) -> View msg -> Element msg
view { theme } source data =
    let
        defaultInternalConfig =
            { iconButtonType = Default
            , textColor = Nothing
            }

        internalConfig =
            toInternalView source defaultInternalConfig

        disabled =
            data.onPress == Nothing

        dynamicAttributes =
            if disabled then
                Font.color theme.colors.gray.color
                    :: focused []
                    :: Element.htmlAttribute (Html.Attributes.style "cursor" "default")
                    :: (case internalConfig.iconButtonType of
                            Default ->
                                []

                            Filled ->
                                [ Background.color theme.colors.gray.lightest ]
                       )

            else
                case internalConfig.iconButtonType of
                    Default ->
                        [ Font.color (Maybe.withDefault black internalConfig.textColor)
                        , Element.htmlAttribute (Html.Attributes.class "ripple focusGrayRipple")
                        , focused [ Background.color theme.colors.gray.lighter ]
                        , mouseOver [ Background.color theme.colors.gray.lightest ]
                        ]

                    Filled ->
                        [ Element.htmlAttribute (Html.Attributes.style "box-shadow" "0 1px 1.5px 0 rgba(0,0,0,.12), 0 1px 1px 0 rgba(0,0,0,.24)")
                        , Font.color (Maybe.withDefault (Color.toElement (Color.textColor theme.colors.secondary.original)) internalConfig.textColor)
                        , Element.htmlAttribute (Html.Attributes.class "ripple focusWhiteRipple")
                        , Background.color theme.colors.secondary.color
                        , focused [ Background.color theme.colors.secondary.light ]
                        , mouseOver [ Background.color (theme.colors.secondary.withAlpha 0.88) ]
                        ]

        attributes =
            toElement source
    in
    Element.el attributes
        (Input.button
            ([ height (px 48)
             , width (px 48)
             , Element.htmlAttribute (Html.Attributes.style "border-radius" "50%")
             , Font.family [ theme.font.main ]
             , Font.size 24
             , Font.bold
             , Font.center
             , padding 12
             ]
                ++ dynamicAttributes
                ++ attributes
            )
            { onPress = data.onPress, label = Element.el [ centerX, centerY ] (icon data.icon) }
        )
