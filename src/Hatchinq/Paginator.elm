module Hatchinq.Paginator exposing (Config, View, configure)

{-|


# Exposed

@docs Config, View, configure

-}

import Element exposing (Element, alignRight, fill, focused, height, htmlAttribute, padding, paddingEach, pointer, px, spacing, width)
import Element.Background
import Element.Events exposing (..)
import Element.Font as Font
import Hatchinq.Attribute exposing (Attribute, toElement)
import Hatchinq.Theme exposing (Theme, icon)
import Hatchinq.Util exposing (enterKeyCode, keyDownAttribute)
import Html.Attributes



-- CONFIG


{-| -}
type alias Config =
    { theme : Theme
    }


{-| -}
configure : Config -> List (Attribute v) -> View msg -> Element msg
configure config =
    view config



-- VIEW


{-| -}
type alias View msg =
    { rowsPerPage : Int
    , offset : Int
    , total : Int
    , nextPage : msg
    , previousPage : msg
    }


view : Config -> List (Attribute v) -> View msg -> Element msg
view { theme } attributes data =
    let
        from =
            max (min (data.offset + 1) data.total) 1

        to =
            max (min (data.offset + data.rowsPerPage) data.total) 1

        elementAttributes =
            toElement attributes

        pagesText =
            if data.total == 0 then
                "0-0 of " ++ String.fromInt data.total

            else
                String.fromInt from ++ "-" ++ String.fromInt to ++ " of " ++ String.fromInt data.total

        nextPageButtonAttributes =
            if to >= data.total then
                [ Font.color theme.colors.gray.color ]

            else
                [ pointer
                , htmlAttribute <| Html.Attributes.attribute "tabindex" "0"
                , htmlAttribute <| Html.Attributes.style "border-radius" "50%"
                , htmlAttribute <| Html.Attributes.class "ripple focusGrayRipple"
                , focused [ Element.Background.color theme.colors.gray.lighter ]
                , onClick data.nextPage
                , keyDownAttribute enterKeyCode data.nextPage
                ]

        previousPageButtonAttributes =
            if from == 1 then
                [ Font.color theme.colors.gray.color ]

            else
                [ pointer
                , htmlAttribute <| Html.Attributes.attribute "tabindex" "0"
                , htmlAttribute <| Html.Attributes.style "border-radius" "50%"
                , htmlAttribute <| Html.Attributes.class "ripple focusGrayRipple"
                , focused [ Element.Background.color theme.colors.gray.lighter ]
                , onClick data.previousPage
                , keyDownAttribute enterKeyCode data.nextPage
                ]
    in
    Element.row
        ([ width fill
         , spacing 24
         , padding 16
         , Font.family [ theme.font.main ]
         , Font.size theme.font.smallerSize
         , Font.color (theme.colors.gray.withAlpha 0.54)
         ]
            ++ elementAttributes
        )
        [ Element.el [ alignRight, paddingEach { top = 0, bottom = 0, left = 0, right = 6 } ]
            (Element.text ("Rows per page: " ++ String.fromInt data.rowsPerPage))
        , Element.el
            [ alignRight, paddingEach { top = 0, bottom = 0, left = 0, right = 20 } ]
            (Element.text pagesText)
        , Element.el
            ([ alignRight, width (px 24), height (px 24) ] ++ previousPageButtonAttributes)
            (icon "chevron_left")
        , Element.el
            ([ alignRight, width (px 24), height (px 24) ] ++ nextPageButtonAttributes)
            (icon "chevron_right")
        ]
