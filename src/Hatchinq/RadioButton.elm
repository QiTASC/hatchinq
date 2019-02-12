module Hatchinq.RadioButton exposing (Config, configure, stopPropagation)

{-|


# Exposed

@docs Config, configure, stopPropagation

-}

import Element exposing (Element, behindContent, centerX, centerY, el, focused, height, html, htmlAttribute, inFront, mouseOver, onRight, pointer, px, width)
import Element.Background as Background
import Element.Font as Font
import Hatchinq.Attribute exposing (Attribute, custom, toElement, toInternalConfig)
import Hatchinq.Theme as Theme exposing (Theme)
import Hatchinq.Util exposing (enterKeyCode, keyDownAttribute)
import Html
import Html.Attributes
import Html.Events
import Json.Decode



-- TYPES


{-| -}
type alias Config =
    { theme : Theme
    }


type alias InternalConfig =
    { stopPropagation : Bool
    }


{-| -}
stopPropagation : Attribute InternalConfig
stopPropagation =
    custom (\v -> { v | stopPropagation = True })



-- VIEW


type alias View msg =
    { value : Bool
    , onChange : Maybe (Bool -> msg)
    }


{-| -}
configure : Config -> (List (Attribute InternalConfig) -> View msg -> Element msg)
configure config =
    view config


view : Config -> List (Attribute InternalConfig) -> View msg -> Element msg
view { theme } attributes data =
    let
        defaultConfig =
            { stopPropagation = False
            }

        internalConfig =
            toInternalConfig attributes defaultConfig

        widthLength =
            px 40

        heightLength =
            px 40

        elementAttributes =
            toElement attributes

        disabled =
            data.onChange == Nothing

        radioButtonDisabledAttributes =
            case data.onChange of
                Just onChange ->
                    [ htmlAttribute <|
                        Html.Events.custom "click"
                            (Json.Decode.succeed
                                { message = onChange (not data.value)
                                , stopPropagation = internalConfig.stopPropagation
                                , preventDefault = False
                                }
                            )
                    ]

                Nothing ->
                    [ htmlAttribute <| Html.Attributes.disabled True ]

        accentColor =
            if data.value then
                theme.colors.secondary.color

            else
                theme.colors.gray.dark

        accentColorType =
            if data.value then
                theme.colors.secondary

            else
                theme.colors.gray

        icon =
            if data.value then
                Theme.icon "radio_button_checked"

            else
                Theme.icon "radio_button_unchecked"

        styleAttributes =
            if disabled then
                []

            else
                [ focused [ Background.color (accentColorType.withAlpha 0.08) ]
                , mouseOver [ Background.color (accentColorType.withAlpha 0.04) ]
                , htmlAttribute <| Html.Attributes.attribute "tabindex" "0"
                , pointer
                ]

        keyEnterAttributes =
            case data.onChange of
                Just onChange ->
                    [ keyDownAttribute enterKeyCode (onChange (not data.value)) ]

                Nothing ->
                    []
    in
    el
        ([ width widthLength
         , height heightLength
         , htmlAttribute <| Html.Attributes.style "border-radius" "50%"
         , htmlAttribute <| Html.Attributes.class "ripple focusSecondaryRipple"
         , behindContent
            (el
                [ centerX
                , centerY
                , Font.color
                    (if disabled then
                        theme.colors.gray.color

                     else
                        accentColor
                    )
                ]
                icon
            )
         ]
            ++ styleAttributes
            ++ radioButtonDisabledAttributes
            ++ keyEnterAttributes
            ++ elementAttributes
        )
        Element.none
