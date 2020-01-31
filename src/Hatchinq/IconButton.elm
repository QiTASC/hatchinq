module Hatchinq.IconButton exposing (Config, View, configure, filled, stopPropagation, withTextColor, white)

{-|


# Exposed

@docs Config, View, configure, filled, stopPropagation, withTextColor, white

-}

import Element exposing (Color, Element, centerX, centerY, focused, height, mouseOver, px, width)
import Element.Background as Background
import Element.Font as Font
import Hatchinq.Attribute exposing (Attribute, custom, toElement, toInternalConfig)
import Hatchinq.Color as Color
import Hatchinq.Theme as Theme exposing (Theme, black, icon)
import Hatchinq.Util exposing (onClickWithoutPropagation)
import Html.Attributes



-- CONFIG


type IconButtonType
    = Default
    | White
    | Filled


type alias InternalConfig =
    { iconButtonType : IconButtonType
    , stopPropagation : Bool
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
stopPropagation : Attribute InternalConfig
stopPropagation =
    custom (\v -> { v | stopPropagation = True })


{-| -}
filled : Attribute InternalConfig
filled =
    custom (\v -> { v | iconButtonType = Filled })


{-| -}
white : Attribute InternalConfig
white =
    custom (\v -> { v | iconButtonType = White })


{-| -}
withTextColor : Color -> Attribute InternalConfig
withTextColor color =
    custom (\v -> { v | textColor = Just color })


view : Config -> List (Attribute InternalConfig) -> View msg -> Element msg
view { theme } source data =
    let
        defaultInternalConfig =
            { iconButtonType = Default
            , stopPropagation = False
            , textColor = Nothing
            }

        internalConfig =
            toInternalConfig source defaultInternalConfig

        disabled =
            data.onPress == Nothing

        dynamicAttributes =
            if disabled then
                Font.color theme.colors.gray.color
                    :: Element.htmlAttribute (Html.Attributes.style "cursor" "default")
                    :: (case internalConfig.iconButtonType of
                            Default ->
                                []

                            White ->
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

                    White ->
                        [ Font.color (Maybe.withDefault Theme.white internalConfig.textColor)
                        , Element.htmlAttribute (Html.Attributes.class "ripple focusWhiteRipple")
                        , focused [ Background.color (Color.toElement (Color.rgba 255 255 255 0.08)) ]
                        , mouseOver [ Background.color (Color.toElement (Color.rgba 255 255 255 0.04)) ]
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

        onClickMsg =
            case data.onPress of
                Nothing ->
                    []

                Just msg ->
                    [ onClickWithoutPropagation internalConfig.stopPropagation msg ]
    in
    Element.el attributes
        (Element.el
            ([ height (px (theme.sizes.minRowHeight - 8))
             , width (px (theme.sizes.minRowHeight - 8))
             , Element.htmlAttribute (Html.Attributes.style "border-radius" "50%")
             , Font.family [ theme.font.main ]
             , Font.size 24
             , Font.bold
             , Font.center
             ]
                ++ dynamicAttributes
                ++ attributes
                ++ onClickMsg
            )
            (Element.el [ centerX, centerY ] (icon data.icon))
        )
