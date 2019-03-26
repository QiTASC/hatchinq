module Hatchinq.ProgressIndicator exposing (Config, Progress(..), GrowthDirection(..), circular, configure, linear, startDelaySeconds, visibility)

{-|


# Exposed

@docs Config, Progress, GrowthDirection, circular, configure, growth, linear, startDelaySeconds, visibility

-}

import Element exposing (Element, fill, height, px, width)
import Element.Background as Background
import Hatchinq.Attribute exposing (Attribute, custom, toElement, toInternalConfig)
import Hatchinq.Theme exposing (Theme)
import Html.Attributes



-- TYPES


{-| -}
type GrowthDirection
    = TopDown
    | BottomUp


type ProgressIndicatorType
    = Linear GrowthDirection
    | Circular


{-| -}
type alias Config =
    { theme : Theme
    }


{-| -}
configure : Config -> (List (Attribute InternalConfig) -> View -> Element msg)
configure config =
    view config


type alias InternalConfig =
    { visible : Bool
    , progressIndicatorType : ProgressIndicatorType
    , startDelaySeconds : Float
    }


{-| -}
type Progress
    = Indeterminate
    | Determinate Float



-- VIEW


{-| -}
type alias View =
    { progress : Progress
    }


{-| -}
linear : GrowthDirection -> Attribute InternalConfig
linear growthDirection =
    custom (\v -> { v | progressIndicatorType = Linear growthDirection })


{-| -}
circular : Attribute InternalConfig
circular =
    custom (\v -> { v | progressIndicatorType = Circular })


{-| -}
visibility : Bool -> Attribute InternalConfig
visibility visible =
    custom (\v -> { v | visible = visible })


{-| -}
startDelaySeconds : Float -> Attribute InternalConfig
startDelaySeconds seconds =
    custom (\v -> { v | startDelaySeconds = seconds })


view : Config -> List (Attribute InternalConfig) -> View -> Element msg
view { theme } attributes { progress } =
    let
        defaultInternalConfig =
            { visible = True
            , progressIndicatorType = Linear TopDown
            , startDelaySeconds = 0
            }

        internalConfig =
            toInternalConfig attributes defaultInternalConfig

        externalAttributes =
            toElement attributes

        bar =
            case progress of
                Determinate pct ->
                    let
                        pctClamped =
                            clamp 0 100 pct
                    in
                    [ Element.el
                        [ Background.color theme.colors.secondary.color
                        , Element.htmlAttribute <| Html.Attributes.style "position" "absolute"
                        , Element.htmlAttribute <| Html.Attributes.style "width" (String.fromFloat pctClamped ++ "%")
                        , Element.htmlAttribute <| Html.Attributes.style "height" "100%"
                        ]
                        Element.none
                    ]

                Indeterminate ->
                    [ Element.el
                        [ Element.htmlAttribute <| Html.Attributes.style "position" "absolute"
                        , Element.htmlAttribute <| Html.Attributes.style "width" "100%"
                        , Element.htmlAttribute <| Html.Attributes.style "height" "100%"
                        , Element.htmlAttribute <| Html.Attributes.class "progress-indicator-primary-bar"
                        ]
                        (Element.el
                            [ Background.color theme.colors.secondary.color
                            , Element.htmlAttribute <| Html.Attributes.style "position" "absolute"
                            , Element.htmlAttribute <| Html.Attributes.style "width" "100%"
                            , Element.htmlAttribute <| Html.Attributes.style "height" "100%"
                            , Element.htmlAttribute <| Html.Attributes.class "progress-indicator-primary-bar-inner"
                            ]
                            Element.none
                        )
                    , Element.el
                        [ Element.htmlAttribute <| Html.Attributes.style "position" "absolute"
                        , Element.htmlAttribute <| Html.Attributes.style "width" "100%"
                        , Element.htmlAttribute <| Html.Attributes.style "height" "100%"
                        , Element.htmlAttribute <| Html.Attributes.class "progress-indicator-secondary-bar"
                        ]
                        (Element.el
                            [ Background.color theme.colors.secondary.color
                            , Element.htmlAttribute <| Html.Attributes.style "position" "absolute"
                            , Element.htmlAttribute <| Html.Attributes.style "width" "100%"
                            , Element.htmlAttribute <| Html.Attributes.style "height" "100%"
                            , Element.htmlAttribute <| Html.Attributes.class "progress-indicator-secondary-bar-inner"
                            ]
                            Element.none
                        )
                    ]

        progressIndicatorAttributes =
            if not internalConfig.visible then
                [ Element.htmlAttribute <| Html.Attributes.style "transform" "scaleY(0)" ]

            else
                []

        transformOrigin =
            case internalConfig.progressIndicatorType of
                Linear TopDown ->
                    "top"

                Linear BottomUp ->
                    "bottom"

                _ ->
                    "top"

        transitionDelaySeconds =
            if not internalConfig.visible then
                0

            else
                internalConfig.startDelaySeconds
    in
    Element.row
        ([ height (px 4)
         , width fill
         , Background.color theme.colors.gray.lighter
         , Element.clipX
         , Element.htmlAttribute <| Html.Attributes.style "transform-origin" transformOrigin
         , Element.htmlAttribute <| Html.Attributes.style "transition" ("transform .5s " ++ String.fromFloat transitionDelaySeconds ++ "s")
         ]
            ++ progressIndicatorAttributes
            ++ externalAttributes
        )
        bar
