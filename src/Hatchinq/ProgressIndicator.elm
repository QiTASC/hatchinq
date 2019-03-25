module Hatchinq.ProgressIndicator exposing (Config, Progress(..), circular, configure, linear, visibility)

{-|


# Exposed

@docs Config, Progress, circular, configure, linear, visibility

-}

import Element exposing (Element, fill, height, px, width)
import Element.Background as Background
import Hatchinq.Attribute exposing (Attribute, custom, toInternalConfig)
import Hatchinq.Theme exposing (Theme)
import Html.Attributes



-- TYPES


type ProgressIndicatorType
    = Linear
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
linear : Attribute InternalConfig
linear =
    custom (\v -> { v | progressIndicatorType = Linear })


{-| -}
circular : Attribute InternalConfig
circular =
    custom (\v -> { v | progressIndicatorType = Circular })


{-| -}
visibility : Bool -> Attribute InternalConfig
visibility visible =
    custom (\v -> { v | visible = visible })


view : Config -> List (Attribute InternalConfig) -> View -> Element msg
view { theme } attributes { progress } =
    let
        defaultInternalConfig =
            { visible = True
            , progressIndicatorType = Linear
            }

        internalConfig =
            toInternalConfig attributes defaultInternalConfig

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
         , Element.htmlAttribute <| Html.Attributes.style "transform-origin" "top"
         , Element.htmlAttribute <| Html.Attributes.style "transition" "transform .5s"
         ]
            ++ progressIndicatorAttributes
        )
        bar
