module Hatchinq.Theme exposing
    ( ColorTheme, ColorType, FontTheme, Theme
    , arrowTransition, black, default, dense, font, icon, lightenOrDarken, stylesheet, textWithEllipsis, transition, transparent, white, withColors
    , IconsResource(..), withIcons)

{-|


# Exposed

@docs ColorTheme, ColorType, FontTheme, Theme
@docs arrowTransition, black, default, dense, font, icon, lightenOrDarken, stylesheet, textWithEllipsis, transition, transparent, white, withColors, withIcons

-}

import Color
import Element exposing (Attribute, Color, Element, Length, el, fill, height, html, htmlAttribute, paddingXY, width)
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
    { rowHeight : Int
    , rowPadding : { top : Int, right : Int, bottom : Int, left : Int }
    , expansionPadding : { top : Int, right : Int, bottom : Int, left : Int }
    , cellPadding : { top : Int, right : Int, bottom : Int, left : Int }
    }


type alias Sizes =
    { minRowHeight : Int
    , table : Table
    }

{-| -}
type IconsResource =
    External String | Css String

{-| -}
type alias Theme =
    { font : FontTheme
    , colors : ColorTheme
    , sizes : Sizes
    , icons : IconsResource
    }



-- COLORS


mqWhite =
    rgba 255 255 255 1


mqBlack =
    rgba 0 0 0 1


{-| -}
white : Color
white =
    toElement mqWhite


{-| -}
black : Color
black =
    toElement mqBlack


darkGray =
    toElement <| rgba 0 0 0 0.6


mqGray =
    rgba 0 0 0 0.24


gray =
    toElement <| mqGray


lightGray =
    toElement <| rgba 0 0 0 0.12


lighterGray =
    toElement <| rgba 0 0 0 0.08


lightestGray =
    toElement <| rgba 0 0 0 0.04


{-| -}
transparent : Color
transparent =
    toElement <| rgba 0 0 0 0



-- FONTS


{-| -}
font : Font
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
withIcons : IconsResource -> Theme -> Theme
withIcons iconsResource theme =
    { theme | icons = iconsResource }


{-| -}
dense : Theme -> Theme
dense theme =
    { theme
        | sizes =
            { minRowHeight = 32
            , table =
                { rowHeight = 32
                , rowPadding = { top = 8, bottom = 8, left = 4, right = 4 }
                , expansionPadding = { top = 8, bottom = 8, left = 12, right = 12 }
                , cellPadding = { top = 0, bottom = 0, left = 8, right = 8 }
                }
            }
    }


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
        { minRowHeight = 48
        , table =
            { rowHeight = 48
            , rowPadding = { top = 12, bottom = 12, left = 4, right = 4 }
            , expansionPadding = { top = 12, bottom = 12, left = 12, right = 12 }
            , cellPadding = { top = 0, bottom = 0, left = 8, right = 8 }
            }
        }
    , icons = External "https://fonts.googleapis.com/icon?family=Material+Icons"
    }



-- STYLESHEET


{-| -}
stylesheet : Theme -> Element msg
stylesheet theme =
    Element.html <|
        Html.div []
            [ case theme.icons of
                External link ->
                    Html.node "link" [ Attr.rel "stylesheet", Attr.href link ] []

                Css css ->
                    Html.node "style" [] [ Html.text css ]

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

                        .button.appbar {
                            overflow: visible;
                        }

                        .button.appbar:after {
                            transform: scale(7, 7);
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

                        .progress-indicator-primary-bar {
                            display: block;
                            transform-origin: top left;
                            left: -145.166611%;

                            -webkit-animation: progress-indicator-primary-bar-translate 2s infinite linear;
                            -webkit-transition: none;
                            -webkit-transform: scaleX(0);

                            animation: progress-indicator-primary-bar-translate 2s infinite linear;
                            transition: none;
                            transform: scaleX(0);
                        }

                        @keyframes progress-indicator-primary-bar-translate {
                            0%      {
                                -webkit-transform: translateX(0);
                                transform: translateX(0);
                            }

                            20%     {
                                -webkit-animation-timing-function: cubic-bezier(.5,0,.701732,.495819);
                                animation-timing-function: cubic-bezier(.5,0,.701732,.495819);
                                -webkit-transform: translateX(0);
                                transform: translateX(0);
                            }

                            59.15%  {
                                -webkit-animation-timing-function: cubic-bezier(.302435,.381352,.55,.956352);
                                animation-timing-function: cubic-bezier(.302435,.381352,.55,.956352);
                                -webkit-transform: translateX(83.67142%);
                                transform: translateX(83.67142%);
                            }

                            100%    {
                                -webkit-transform: translateX(200.611057%);
                                transform: translateX(200.611057%);
                            }
                        }

                        .progress-indicator-primary-bar-inner {
                            display: inline-block;

                            -webkit-animation: progress-indicator-primary-bar-scale 2s infinite linear;
                            animation: progress-indicator-primary-bar-scale 2s infinite linear;
                        }

                        @keyframes progress-indicator-primary-bar-scale {
                            0%      {
                                -webkit-transform: scaleX(.08);
                                transform: scaleX(.08);
                            }
                            36.65%  {
                                -webkit-animation-timing-function: cubic-bezier(.334731,.12482,.785844,1);
                                animation-timing-function: cubic-bezier(.334731,.12482,.785844,1);
                                -webkit-transform: scaleX(.08);
                                transform: scaleX(.08);
                            }

                            69.15%  {
                                -webkit-animation-timing-function: cubic-bezier(.06,.11,.6,1);
                                animation-timing-function: cubic-bezier(.06,.11,.6,1);
                                -webkit-transform: scaleX(.661479);
                                transform: scaleX(.661479);
                            }
                            100%    {
                                -webkit-transform: scaleX(.08);
                                transform: scaleX(.08);
                            }
                        }

                        .progress-indicator-secondary-bar {
                            display: block;
                            transform-origin: top left;
                            left: -54.888891%;

                            -webkit-animation: progress-indicator-secondary-bar-translate 2s infinite linear;
                            -webkit-transition: none;
                            -webkit-transform: scaleX(0);

                            animation: progress-indicator-secondary-bar-translate 2s infinite linear;
                            transition: none;
                            transform: scaleX(0);
                        }

                        @keyframes progress-indicator-secondary-bar-translate {
                            0%      {
                                -webkit-animation-timing-function: cubic-bezier(.15,0,.515058,.409685);
                                animation-timing-function: cubic-bezier(.15,0,.515058,.409685);
                                -webkit-transform: translateX(0);
                                transform: translateX(0);
                            }

                            25%     {
                                -webkit-animation-timing-function: cubic-bezier(.31033,.284058,.8,.733712);
                                animation-timing-function: cubic-bezier(.31033,.284058,.8,.733712);
                                -webkit-transform: translateX(37.651913%);
                                transform: translateX(37.651913%);
                            }
                            48.35%  {
                                -webkit-animation-timing-function: cubic-bezier(.4,.627035,.6,.902026);
                                animation-timing-function: cubic-bezier(.4,.627035,.6,.902026);
                                -webkit-transform: translateX(84.386165%);
                                transform: translateX(84.386165%);
                            }
                            100%    {
                                -webkit-transform: translateX(160.277782%);
                                transform: translateX(160.277782%);
                            }
                        }

                        .progress-indicator-secondary-bar-inner {
                            display: inline-block;

                            -webkit-animation: progress-indicator-secondary-bar-scale 2s infinite linear;
                            animation: progress-indicator-secondary-bar-scale 2s infinite linear;
                        }

                        @keyframes progress-indicator-secondary-bar-scale {
                            0%      {
                                -webkit-animation-timing-function: cubic-bezier(.205028,.057051,.57661,.453971);
                                animation-timing-function: cubic-bezier(.205028,.057051,.57661,.453971);
                                -webkit-transform: scaleX(.08);
                                transform: scaleX(.08);
                            }

                            19.15%  {
                                -webkit-animation-timing-function: cubic-bezier(.152313,.196432,.648374,1.004315);
                                animation-timing-function: cubic-bezier(.152313,.196432,.648374,1.004315);
                                -webkit-transform: scaleX(.457104);
                                transform: scaleX(.457104);
                            }
                            44.15%  {
                                -webkit-animation-timing-function: cubic-bezier(.257759,-.003163,.211762,1.38179);
                                animation-timing-function: cubic-bezier(.257759,-.003163,.211762,1.38179);
                                -webkit-transform: scaleX(.72796);
                                transform: scaleX(.72796);
                            }
                            100%    {
                                -webkit-transform: scaleX(.08);
                                transform: scaleX(.08);
                            }
                        }

                        .progress-indicator-circular-container {
                            display: inline-block !important;
                            position: relative !important;

                            -webkit-animation: progress-indicator-circular-container-rotate 1568ms linear infinite;
                            animation: progress-indicator-circular-container-rotate 1568ms linear infinite;
                        }

                        @keyframes progress-indicator-circular-container-rotate {
                            100%    {
                                -webkit-transform: rotate(360deg);
                                transform: rotate(360deg);
                            }
                        }

                        .progress-indicator-circular-spinner {
                            position: absolute !important;
                            width: 100% !important;
                            height: 100% !important;
                            transition: opacity .5s;
                            border-color: inherit;

                            -webkit-animation: progress-indicator-circular-spinner-rotate 5332ms cubic-bezier(0.4, 0, 0.2, 1) infinite both;
                            animation: progress-indicator-circular-spinner-rotate 5332ms cubic-bezier(0.4, 0, 0.2, 1) infinite both;
                        }

                        @keyframes progress-indicator-circular-spinner-rotate {
                            12.5%   {
                                -webkit-transform: rotate(135deg);
                                transform: rotate(135deg);
                            }
                            25%     {
                                -webkit-transform: rotate(270deg);
                                transform: rotate(270deg);
                            }
                            37.5%   {
                                -webkit-transform: rotate(405deg);
                                transform: rotate(405deg);
                            }
                            50%     {
                                -webkit-transform: rotate(540deg);
                                transform: rotate(540deg);
                            }
                            62.5%   {
                                -webkit-transform: rotate(675deg);
                                transform: rotate(675deg);
                            }
                            75%     {
                                -webkit-transform: rotate(810deg);
                                transform: rotate(810deg);
                            }
                            87.5%   {
                                -webkit-transform: rotate(945deg);
                                transform: rotate(945deg);
                            }
                            100%    {
                                -webkit-transform: rotate(1080deg);
                                transform: rotate(1080deg);
                            }
                        }

                        .circle {
                            border-radius: 50%;
                        }

                        .progress-indicator-circular-determinate-container {
                            position: relative !important;
                        }

                        .progress-indicator-circular-determinate-container.lt50 {
                            clip-path: polygon(50% 0%, 50% 100%, 100% 100%, 100% 0%);
                        }

                        .progress-indicator-circular-determinate-container .circle {
                            width: 100% !important;
                            height: 100% !important;
                            border-width: 4px;
                            border-style: solid;
                            border-color: inherit;
                            border-radius: 50%;
                            -webkit-animation: none;
                            animation: none;
                            position: absolute;
                        }

                        .progress-indicator-circular-determinate-container .circle.half {
                            clip-path: polygon(50% 0%, 50% 100%, 0% 100%, 0% 0%);
                        }

                        .circle-clipper {
                            flex-basis: auto !important;
                            display: inline-block !important;
                            position: relative !important;
                            width: 50% !important;
                            height: 100% !important;
                            overflow: hidden;
                            border-color: inherit;
                        }

                        .circle-clipper .circle {
                            width: 200% !important;
                            height: 100% !important;
                            border-width: 4px;
                            border-style: solid;
                            border-color: inherit;
                            border-bottom-color: transparent !important;
                            border-radius: 50%;
                            -webkit-animation: none;
                            animation: none;
                            position: absolute;
                            top: 0;
                            right: 0;
                            bottom: 0;
                        }

                        .left {
                            float: left !important;
                        }

                        .right {
                            float: right !important;
                        }

                        .circle-clipper.left .circle {
                            left: 0;
                            border-right-color: transparent !important;
                            -webkit-transform: rotate(129deg);
                            transform: rotate(129deg);

                            -webkit-animation: left-spin 1333ms cubic-bezier(0.4, 0, 0.2, 1) infinite both;
                            animation: left-spin 1333ms cubic-bezier(0.4, 0, 0.2, 1) infinite both;
                        }

                        @keyframes left-spin {
                            0% {
                                -webkit-transform: rotate(130deg);
                                transform: rotate(130deg);
                            }

                            50% {
                                -webkit-transform: rotate(-5deg);
                                transform: rotate(-5deg);
                            }
                            100% {
                                -webkit-transform: rotate(130deg);
                                transform: rotate(130deg);
                            }
                        }

                        .circle-clipper.right .circle {
                            left: -100%;
                            border-left-color: transparent !important;
                            -webkit-transform: rotate(-129deg);
                            transform: rotate(-129deg);

                            -webkit-animation: right-spin 1333ms cubic-bezier(0.4, 0, 0.2, 1) infinite both;
                            animation: right-spin 1333ms cubic-bezier(0.4, 0, 0.2, 1) infinite both;
                        }

                        @keyframes right-spin {
                            0% {
                                -webkit-transform: rotate(-130deg);
                                transform: rotate(-130deg);
                            }
                            50% {
                                -webkit-transform: rotate(5deg);
                                transform: rotate(5deg);
                            }
                            100% {
                                -webkit-transform: rotate(-130deg);
                                transform: rotate(-130deg);
                            }
                        }

                        .gap-patch {
                            position: absolute !important;
                            top: 0;
                            left: 45%;
                            width: 10% !important;
                            height: 100% !important;
                            overflow: hidden;
                            border-color: inherit;
                        }

                        .gap-patch .circle {
                            width: 1000%;
                            left: -450%;
                        }
                    """)
                ]
            ]



-- UTILITY


{-| -}
icon : String -> Element msg
icon name =
    Html.i [ Attr.class "material-icons", Attr.style "user-select" "none" ] [ Html.text name ] |> html


{-| -}
transition : Html.Attribute msg
transition =
    Attr.style "transition" "all .25s"


{-| -}
arrowTransition : Html.Attribute msg
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
        , htmlAttribute <| Attr.title text
        ]
        (html <| Html.text text)


{-| -}
lightenOrDarken : Element.Color -> Float -> Element.Color
lightenOrDarken color amount =
    let
        rgb =
            Element.toRgb color

        hsl =
            Color.toHsla <| Color.fromRgba { red = rgb.red, green = rgb.green, blue = rgb.blue, alpha = rgb.alpha }

        newHsl =
            { hsl | lightness = hsl.lightness + amount }

        newColor =
            Color.toRgba <| Color.fromHsla newHsl
    in
    Element.fromRgb { red = newColor.red, green = newColor.green, blue = newColor.blue, alpha = newColor.alpha }
