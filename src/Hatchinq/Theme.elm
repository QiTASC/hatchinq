module Hatchinq.Theme exposing (ColorTheme, ColorType, FontTheme, Theme, arrowTransition, black, default, font, icon, lightGray, stylesheet, textWithEllipsis, transition, transparent, white, withColors)

{-|


# Exposed

@docs ColorTheme, ColorType, FontTheme, Theme, arrowTransition, black, default, font, icon, lightGray, stylesheet, textWithEllipsis, transition, transparent, white, withColors

-}

import Element exposing (Attribute, Color, Element, Length, el, fill, height, html, htmlAttribute, paddingXY, px, width)
import Element.Font exposing (Font)
import Hatchinq.Color as QColor exposing (alpha, blue, green, isBrighter, red, rgba, toElement, withAlpha)
import Html exposing (Html)
import Html.Attributes as Attr


{-| -}
type alias FontTheme =
    { main : Font
    , defaultSize : Int
    , smallSize : Int
    , smallerSize : Int
    , smallestSize : Int
    }


{-| -}
type alias ColorType =
    { original : QColor.Color
    , color : Color
    , textColor : Color
    , lightest : Color
    , lighter : Color
    , light : Color
    , dark : Color
    , withAlpha : Float -> Color
    }


{-| -}
type alias ColorTheme =
    { primary : ColorType
    , secondary : ColorType
    , gray : ColorType
    }


type alias Table =
    { rowHeight : Length
    , rowPadding : { top : Int, right : Int, bottom : Int, left : Int }
    , expansionPadding : { top : Int, right : Int, bottom : Int, left : Int }
    , cellPadding : { top : Int, right : Int, bottom : Int, left : Int }
    }


type alias Sizes =
    { table : Table
    }


{-| -}
type alias Theme =
    { font : FontTheme
    , colors : ColorTheme
    , sizes : Sizes
    }



-- COLORS


mqWhite =
    rgba 255 255 255 1


mqBlack =
    rgba 0 0 0 1


{-| -}
white =
    toElement mqWhite


{-| -}
black =
    toElement mqBlack


darkGray =
    toElement <| rgba 0 0 0 0.6


mqGray =
    rgba 0 0 0 0.24


gray =
    toElement <| mqGray


{-| -}
lightGray =
    toElement <| rgba 0 0 0 0.12


lighterGray =
    toElement <| rgba 0 0 0 0.08


lightestGray =
    toElement <| rgba 0 0 0 0.04


{-| -}
transparent =
    toElement <| rgba 0 0 0 0



-- FONTS


{-| -}
font =
    Element.Font.typeface "Avenir"



-- DEFAULTS


colors : QColor.Color -> QColor.Color -> ColorTheme
colors primaryColor secondaryColor =
    let
        darker channel =
            round (toFloat channel * 0.77)

        primaryDarkColor =
            rgba (darker (red primaryColor)) (darker (green primaryColor)) (darker (blue primaryColor)) (alpha primaryColor)

        secondaryDarkColor =
            rgba (darker (red secondaryColor)) (darker (green secondaryColor)) (darker (blue secondaryColor)) (alpha primaryColor)

        isBright =
            \color ->
                isBrighter color (withAlpha 0.5 mqWhite)

        textColor =
            \color ->
                if isBright color then
                    mqBlack

                else
                    mqWhite
    in
    { primary =
        { original = primaryColor
        , color = toElement primaryColor
        , textColor = toElement <| textColor primaryColor
        , lightest = toElement <| withAlpha 0.08 primaryColor
        , lighter = toElement <| withAlpha 0.24 primaryColor
        , light = toElement <| withAlpha 0.76 primaryColor
        , dark = toElement <| primaryDarkColor
        , withAlpha = \alpha -> toElement <| withAlpha alpha primaryColor
        }
    , secondary =
        { original = secondaryColor
        , color = toElement secondaryColor
        , textColor = toElement <| textColor secondaryColor
        , lightest = toElement <| withAlpha 0.08 secondaryColor
        , lighter = toElement <| withAlpha 0.24 secondaryColor
        , light = toElement <| withAlpha 0.76 secondaryColor
        , dark = toElement <| secondaryDarkColor
        , withAlpha = \alpha -> toElement <| withAlpha alpha secondaryColor
        }
    , gray =
        { original = mqGray
        , color = toElement mqGray
        , textColor = toElement <| textColor mqGray
        , lightest = lightestGray
        , lighter = lighterGray
        , light = lightGray
        , dark = darkGray
        , withAlpha = \alpha -> toElement <| withAlpha alpha mqBlack
        }
    }


{-| -}
withColors : QColor.Color -> QColor.Color -> Theme -> Theme
withColors primaryColor secondaryColor theme =
    { theme | colors = colors primaryColor secondaryColor }


{-| -}
default : Theme
default =
    let
        primaryColor =
            rgba 247 148 30 1

        secondaryColor =
            rgba 0 155 164 1
    in
    { font =
        { main = font
        , defaultSize = 16
        , smallSize = 14
        , smallerSize = 12
        , smallestSize = 10
        }
    , colors = colors primaryColor secondaryColor
    , sizes =
        { table =
            { rowHeight = px 48
            , rowPadding = { top = 12, bottom = 12, left = 4, right = 4 }
            , expansionPadding = { top = 12, bottom = 12, left = 12, right = 12 }
            , cellPadding = { top = 0, bottom = 0, left = 8, right = 8 }
            }
        }
    }



-- STYLESHEET


{-| -}
stylesheet : Theme -> Element msg
stylesheet theme =
    Element.html <|
        Html.div []
            [ Html.node "link"
                [ Attr.rel "stylesheet"
                , Attr.href "https://fonts.googleapis.com/icon?family=Material+Icons"
                ]
                []
            , Html.node "style"
                []
                [ Html.text
                    ("""
                        ::-webkit-scrollbar {
                            width:6px;
                            height:6px;
                        }

                        ::-webkit-scrollbar-track {
                            -webkit-border-radius:5px;
                            border-radius:5px;
                            background:rgba(0,0,0,0.08);
                        }

                        ::-webkit-scrollbar-thumb {
                            -webkit-border-radius:5px;
                            border-radius:5px;
                            background:rgba(0,0,0,0.24);
                        }

                        ::-webkit-scrollbar-thumb:hover {
                            background:rgba(0,0,0,0.48);
                        }

                        ::-webkit-scrollbar-thumb:window-inactive {
                            background:rgba(0,0,0,0.08);
                        }

                        .focusPrimaryRipple {
                            --ripple-color: rgba(""" ++ String.fromInt (red theme.colors.primary.original) ++ "," ++ String.fromInt (green theme.colors.primary.original) ++ "," ++ String.fromInt (blue theme.colors.primary.original) ++ """,0.24);
                        }

                        .focusSecondaryRipple {
                            --ripple-color: rgba(""" ++ String.fromInt (red theme.colors.secondary.original) ++ "," ++ String.fromInt (green theme.colors.secondary.original) ++ "," ++ String.fromInt (blue theme.colors.secondary.original) ++ """,0.24);
                        }

                        .focusGrayRipple {
                            --ripple-color: rgba(0,0,0,0.24);
                        }

                        .focusWhiteRipple {
                            --ripple-color: rgba(255,255,255,0.24);
                        }

                        .button {
                            position: relative;
                            overflow: hidden;
                            transform: translate3d(0, 0, 0);
                        }

                        .button:after {
                            content: "";
                            display: block;
                            position: absolute;
                            width: 100%;
                            height: 100%;
                            top: 0;
                            left: 0;
                            pointer-events: none;
                            background-image: radial-gradient(circle, var(--ripple-color, #555) 10%, transparent 10.01%);
                            background-repeat: no-repeat;
                            background-position: 50%;
                            transform: scale(10, 10);
                            opacity: 0;
                            transition: transform .25s, opacity 1s;
                        }

                        .button:active:after {
                            transform: scale(0, 0);
                            opacity: 1;
                            transition: 0s;
                        }

                        .ripple {
                            position: relative;
                            overflow: hidden;
                            transform: translate3d(0, 0, 0);
                        }

                        .ripple:before {
                            content: "";
                            display: block;
                            position: absolute;
                            width: 100%;
                            height: 100%;
                            top: 0;
                            left: 0;
                            pointer-events: none;
                            background-image: radial-gradient(circle, var(--ripple-color, #555) 10%, transparent 10.01%);
                            background-repeat: no-repeat;
                            background-position: 50%;
                            transform: scale(0, 0);
                            opacity: 0;
                            transition: opacity .5s, transform 1s .5s;

                        }

                        .ripple:active:before {
                            transform: scale(20, 20);
                            opacity: 1;
                            transition: transform .5s, opacity .25s;
                        }
                    """)
                ]
            ]



-- UTILITY


{-| -}
icon name =
    Html.i [ Attr.class "material-icons", Attr.style "user-select" "none" ] [ Html.text name ] |> html


{-| -}
transition =
    Attr.style "transition" "all .25s"


{-| -}
arrowTransition =
    Attr.style "transition" "all .15s, transform .15s cubic-bezier(0.4, 0, 0.2, 1) 0s, -webkit-transform .15s cubic-bezier(0.4, 0, 0.2, 1) 0s"


{-| -}
textWithEllipsis : String -> Element msg
textWithEllipsis text =
    el
        [ paddingXY 0 2
        , width fill
        , height fill
        , htmlAttribute <| Attr.style "display" "inline-block"
        , htmlAttribute <| Attr.style "overflow" "hidden"
        , htmlAttribute <| Attr.style "text-overflow" "ellipsis"
        ]
        (html <| Html.text text)
