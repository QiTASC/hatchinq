module Hatchinq.IconButton exposing
    ( Config, View, configure, filled, stopPropagation, withTextColor, white
    , fontSize
    )

{-|


# Exposed

@docs Config, View, configure, filled, stopPropagation, withTextColor, white

-}

import Debug exposing (toString)
import Element exposing (Color, Element, centerX, centerY, focused, height, html, mouseOver, px, width)
import Element.Background as Background
import Element.Font as Font
import Hatchinq.Attribute exposing (Attribute, custom, toElement, toInternalConfig)
import Hatchinq.Color as Color
import Hatchinq.Theme as Theme exposing (Theme, black, icon)
import Html
import Html.Attributes
import Html.Events
import Json.Decode as Decode



-- CONFIG


type IconButtonType
    = Default
    | White
    | Filled


type alias InternalConfig =
    { iconButtonType : IconButtonType
    , stopPropagation : Bool
    , textColor : Maybe Color
    , fontSize : Int
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
fontSize : Int -> Attribute InternalConfig
fontSize size =
    custom (\v -> { v | fontSize = size })


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
            , fontSize = 24
            }

        internalConfig =
            toInternalConfig source defaultInternalConfig

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
                    [ onClickNoBubble internalConfig.stopPropagation msg ]
    in
    Element.el attributes
        (Element.el
            ([ height (px (theme.sizes.minRowHeight - 8))
             , width (px (theme.sizes.minRowHeight - 8))
             , Element.htmlAttribute (Html.Attributes.style "border-radius" "50%")
             , Font.family [ theme.font.main ]
             , Font.bold
             , Font.center
             ]
                ++ dynamicAttributes
                ++ attributes
                ++ onClickMsg
            )
            (Element.el [ centerX, centerY ] (Html.i [ Html.Attributes.class "material-icons", Html.Attributes.style "user-select" "none", Html.Attributes.style "font-size" (toString internalConfig.fontSize ++ "px") ] [ Html.text data.icon ] |> html))
        )


onClickNoBubble : Bool -> msg -> Element.Attribute msg
onClickNoBubble noPropagation message =
    Element.htmlAttribute <| Html.Events.custom "click" (Decode.succeed { message = message, stopPropagation = noPropagation, preventDefault = True })
