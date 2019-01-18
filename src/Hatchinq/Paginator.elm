module Hatchinq.Paginator exposing (Config, View, configure)

{-|


# Exposed

@docs Config, View, configure

-}

import Element exposing (..)
import Element.Events exposing (..)
import Element.Font as Font
import Hatchinq.Theme exposing (Theme, icon)



-- CONFIG


{-| -}
type alias Config =
    { theme : Theme
    }


{-| -}
configure : Config -> View msg -> Element msg
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


view : Config -> View msg -> Element msg
view { theme } data =
    let
        from =
            max (min (data.offset + 1) data.total) 1

        to =
            max (min (data.offset + data.rowsPerPage) data.total) 1

        pagesText =
            String.fromInt from ++ "-" ++ String.fromInt to ++ " of " ++ String.fromInt data.total

        nextPageButtonAttributes =
            if to == data.total then
                [ Font.color theme.colors.gray.color ]

            else
                [ pointer, onClick data.nextPage ]

        previousPageButtonAttributes =
            if from == 1 then
                [ Font.color theme.colors.gray.color ]

            else
                [ pointer, onClick data.previousPage ]
    in
    row
        [ width fill
        , spacing 24
        , padding 16
        , Font.family [ theme.font.main ]
        , Font.size theme.font.smallerSize
        , Font.color (theme.colors.gray.withAlpha 0.54)
        ]
        [ el [ alignRight, paddingEach { top = 0, bottom = 0, left = 0, right = 6 } ]
            (text ("Rows per page: " ++ String.fromInt data.rowsPerPage))
        , el
            [ alignRight, paddingEach { top = 0, bottom = 0, left = 0, right = 20 } ]
            (text pagesText)
        , el
            ([ alignRight, width (px 24), height (px 24) ] ++ previousPageButtonAttributes)
            (icon "chevron_left")
        , el
            ([ alignRight, width (px 24), height (px 24) ] ++ nextPageButtonAttributes)
            (icon "chevron_right")
        ]
