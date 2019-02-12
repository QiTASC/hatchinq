module Hatchinq.Checkbox exposing (Config, configure, stopPropagation)

{-|


# Exposed

@docs Config, configure, stopPropagation

-}

import Element exposing (Element, behindContent, centerX, centerY, el, focused, height, html, htmlAttribute, mouseOver, pointer, px, width)
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
    { value : Maybe Bool
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

        elementAttributes =
            toElement attributes

        disabled =
            data.onChange == Nothing

        nativeCheckboxValueAttributes =
            case data.value of
                Just True ->
                    [ Html.Attributes.attribute "aria-checked" "true" ]

                Just False ->
                    [ Html.Attributes.attribute "aria-checked" "false" ]

                Nothing ->
                    [ Html.Attributes.attribute "aria-checked" "mixed" ]

        nativeCheckboxDisabledAttributes =
            case data.onChange of
                Just onChange ->
                    [ Html.Events.custom "click"
                        (Json.Decode.succeed
                            { message = onChange (not (Maybe.withDefault False data.value))
                            , stopPropagation = internalConfig.stopPropagation
                            , preventDefault = False
                            }
                        )
                    ]

                Nothing ->
                    [ Html.Attributes.disabled True ]

        accentColor =
            case data.value of
                Just False ->
                    theme.colors.gray.dark

                _ ->
                    theme.colors.secondary.color

        accentColorType =
            case data.value of
                Just False ->
                    theme.colors.gray

                _ ->
                    theme.colors.secondary

        icon =
            case data.value of
                Just True ->
                    Theme.icon "check_box"

                Just False ->
                    Theme.icon "check_box_outline_blank"

                Nothing ->
                    Theme.icon "indeterminate_check_box"

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
                    [ keyDownAttribute enterKeyCode (onChange (not (Maybe.withDefault False data.value))) ]

                Nothing ->
                    []
    in
    el
        ([ width (px (theme.sizes.minRowHeight - 8))
         , height (px (theme.sizes.minRowHeight - 8))
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
            ++ elementAttributes
            ++ keyEnterAttributes
        )
        (html <|
            Html.input
                ([ Html.Attributes.type_ "checkbox"
                 , Html.Attributes.style "width" "100%"
                 , Html.Attributes.style "height" "100%"
                 , Html.Attributes.style "opacity" "0"
                 , Html.Attributes.style "padding" "0"
                 , Html.Attributes.style "margin" "0"
                 , Html.Attributes.style "position" "absolute"
                 , Html.Attributes.style "top" "0"
                 , Html.Attributes.style "left" "0"
                 , Html.Attributes.style "cursor" "inherit"
                 , Html.Attributes.attribute "tabindex" "-1"
                 ]
                    ++ nativeCheckboxValueAttributes
                    ++ nativeCheckboxDisabledAttributes
                )
                []
        )
