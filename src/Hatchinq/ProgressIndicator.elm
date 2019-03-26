module Hatchinq.ProgressIndicator exposing (Config, Progress(..), GrowthDirection(..), circular, configure, linear, startDelaySeconds, visibility)

{-|


# Exposed

@docs Config, Progress, GrowthDirection, circular, configure, linear, startDelaySeconds, visibility

-}

import Element exposing (Element, fill, height, px, width)
import Element.Background as Background
import Element.Border as Border
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
view { theme } attributes data =
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
    in
    case internalConfig.progressIndicatorType of
        Linear growthDirection ->
            linearProgressIndicator theme growthDirection internalConfig externalAttributes data

        Circular ->
            circularProgressIndicator theme internalConfig externalAttributes data


linearProgressIndicator : Theme -> GrowthDirection -> InternalConfig -> List (Element.Attribute msg) -> View -> Element msg
linearProgressIndicator theme growthDirection internalConfig externalAttributes { progress } =
    let
        transformOrigin =
            case growthDirection of
                TopDown ->
                    "top"

                BottomUp ->
                    "bottom"

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
    in
    Element.row
        ([ height (px 4)
         , width fill
         , Background.color theme.colors.gray.lighter
         , Element.clipX
         , Element.htmlAttribute <| Html.Attributes.style "transform-origin" transformOrigin
         , Element.htmlAttribute <| Html.Attributes.style "transition" ("transform .5s " ++ String.fromFloat (transitionDelaySeconds internalConfig) ++ "s")
         ]
            ++ progressIndicatorAttributes
            ++ externalAttributes
        )
        bar


circularProgressIndicator : Theme -> InternalConfig -> List (Element.Attribute msg) -> View -> Element msg
circularProgressIndicator theme internalConfig externalAttributes { progress } =
    let
        progressIndicatorAttributes =
            if not internalConfig.visible then
                [ Element.htmlAttribute <| Html.Attributes.style "opacity" "0" ]

            else
                []

        defaultDeterminateContainerAttributes =
            [ height (px 45)
            , width (px 45)
            , Border.color theme.colors.secondary.color
            , Element.htmlAttribute <| Html.Attributes.class "progress-indicator-circular-determinate-container"
            ]

        circle =
            case progress of
                Determinate pct ->
                    let
                        pctClamped =
                            clamp 0 100 pct
                    in
                    if pctClamped < 50 then
                        Element.el
                            ([ Element.htmlAttribute <| Html.Attributes.class "lt50" ]
                                ++ defaultDeterminateContainerAttributes
                                ++ externalAttributes
                            )
                            (Element.el
                                ([ Element.htmlAttribute <| Html.Attributes.class "circle"
                                 , Element.htmlAttribute <| Html.Attributes.class "half"
                                 , Element.htmlAttribute <| Html.Attributes.style "transform" ("rotate(" ++ String.fromFloat (pctClamped / 50 * 180) ++ "deg)")
                                 , Element.htmlAttribute <| Html.Attributes.style "transition" ("opacity .5s " ++ String.fromFloat (transitionDelaySeconds internalConfig) ++ "s")
                                 ]
                                    ++ progressIndicatorAttributes
                                )
                                Element.none
                            )

                    else if pctClamped < 100 then
                        Element.row
                            (defaultDeterminateContainerAttributes
                                ++ externalAttributes
                            )
                            [ Element.el
                                ([ Element.htmlAttribute <| Html.Attributes.class "circle"
                                 , Element.htmlAttribute <| Html.Attributes.class "half"
                                 , Element.htmlAttribute <| Html.Attributes.style "transform" ("rotate(" ++ String.fromFloat (pctClamped / 50 * 180) ++ "deg)")
                                 , Element.htmlAttribute <| Html.Attributes.style "transition" ("opacity .5s " ++ String.fromFloat (transitionDelaySeconds internalConfig) ++ "s")
                                 ]
                                    ++ progressIndicatorAttributes
                                )
                                Element.none
                            , Element.el
                                [ Element.htmlAttribute <| Html.Attributes.class "circle"
                                , Element.htmlAttribute <| Html.Attributes.class "half"
                                , Element.htmlAttribute <| Html.Attributes.style "transform" "rotate(180deg)"
                                ]
                                Element.none
                            ]

                    else
                        Element.el
                            (defaultDeterminateContainerAttributes
                                ++ externalAttributes
                            )
                            (Element.el
                                ([ Element.htmlAttribute <| Html.Attributes.class "circle"
                                 , Element.htmlAttribute <| Html.Attributes.style "transition" ("all .25s " ++ String.fromFloat (transitionDelaySeconds internalConfig) ++ "s")
                                 ]
                                    ++ progressIndicatorAttributes
                                )
                                Element.none
                            )

                Indeterminate ->
                    Element.el
                        ([ Element.htmlAttribute <| Html.Attributes.class "progress-indicator-circular-container"
                         , height (px 45)
                         , width (px 45)
                         , Border.color theme.colors.secondary.color
                         ]
                            ++ externalAttributes
                        )
                        (Element.row
                            ([ Element.htmlAttribute <| Html.Attributes.class "progress-indicator-circular-spinner"
                             , Element.htmlAttribute <| Html.Attributes.style "transition" ("opacity .5s " ++ String.fromFloat (transitionDelaySeconds internalConfig) ++ "s")
                             ]
                                ++ progressIndicatorAttributes
                            )
                            [ Element.el
                                [ Element.htmlAttribute <| Html.Attributes.class "circle-clipper"
                                , Element.htmlAttribute <| Html.Attributes.class "left"
                                ]
                                (Element.el [ Element.htmlAttribute <| Html.Attributes.class "circle" ] Element.none)
                            , Element.el
                                [ Element.htmlAttribute <| Html.Attributes.class "gap-patch" ]
                                Element.none
                            , Element.el
                                [ Element.htmlAttribute <| Html.Attributes.class "circle-clipper"
                                , Element.htmlAttribute <| Html.Attributes.class "right"
                                ]
                                (Element.el [ Element.htmlAttribute <| Html.Attributes.class "circle" ] Element.none)
                            ]
                        )
    in
    circle


transitionDelaySeconds : InternalConfig -> Float
transitionDelaySeconds internalConfig =
    if not internalConfig.visible then
        0

    else
        internalConfig.startDelaySeconds
