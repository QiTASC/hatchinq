module Hatchinq.Chip exposing (Config, coloring, configure, icon, maxWidth, withError)

{-|


# Exposed

@docs Config, configure, withError, coloring, icon, maxWidth

-}

import Element exposing (Color, Element, alignBottom, centerY, column, el, fill, maximum, none, paddingEach, pointer, rgb255, row, shrink, spacing, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Hatchinq.Attribute exposing (Attribute, custom, toInternalConfig)
import Hatchinq.Theme exposing (Theme, textWithEllipsis)



-- TYPES


{-| -}
type alias Config =
    { theme : Theme }


type alias InternalConfig item msg =
    { error : Bool
    , coloring : Maybe (item -> Color)
    , icon : Maybe (Element msg)
    , maxWidth : Maybe Int
    }


{-| -}
withError : Bool -> Attribute (InternalConfig item msg)
withError error =
    custom (\v -> { v | error = error })


{-| -}
coloring : (item -> Element.Color) -> Attribute (InternalConfig item msg)
coloring coloring_ =
    custom (\v -> { v | coloring = Just coloring_ })


{-| -}
icon : Element msg -> Attribute (InternalConfig item msg)
icon data =
    custom (\v -> { v | icon = Just data })


{-| -}
maxWidth : Int -> Attribute (InternalConfig item msg)
maxWidth width =
    custom (\v -> { v | maxWidth = Just width })



-- VIEW


type alias View item msg =
    { item : item
    , toString : item -> String
    , onClick : Maybe (item -> msg)
    , onClose : Maybe (item -> msg)
    }


{-| -}
configure : Config -> (List (Attribute (InternalConfig item msg)) -> View item msg -> Element msg)
configure config =
    view config


view : Config -> List (Attribute (InternalConfig item msg)) -> View item msg -> Element msg
view { theme } attributes { item, toString, onClick, onClose } =
    let
        defaultConfig =
            { error = False
            , coloring = Nothing
            , icon = Nothing
            , maxWidth = Nothing
            }

        internalConfig =
            toInternalConfig attributes defaultConfig

        ( backgroundColor, iconColor ) =
            if internalConfig.error then
                ( rgb255 255 204 203, rgb255 169 0 0 )

            else
                ( Maybe.withDefault theme.colors.gray.light <| Maybe.map (\coloring_ -> coloring_ item) internalConfig.coloring
                , theme.colors.gray.dark
                )

        textWidth =
            case internalConfig.maxWidth of
                Just w ->
                    width (fill |> maximum (w - 16 - (Maybe.withDefault 0 <| Maybe.map (\_ -> 32) internalConfig.icon) - (Maybe.withDefault 0 <| Maybe.map (\_ -> 32) onClose)))

                Nothing ->
                    width shrink
    in
    column
        ([ Font.family [ theme.font.main ]
         , Border.rounded 24
         , Background.color backgroundColor
         , paddingEach
            { top = 8
            , bottom = 8
            , left =
                if internalConfig.icon == Nothing then
                    16

                else
                    8
            , right =
                if onClose == Nothing then
                    16

                else
                    8
            }
         , spacing 4
         , alignBottom
         ]
            ++ (Maybe.withDefault [] <| Maybe.map (\click -> [ Element.Events.onClick (click item) ]) onClick)
        )
        [ row
            [ spacing 8, centerY ]
            [ Maybe.withDefault none internalConfig.icon
            , el [ textWidth ] <| textWithEllipsis (toString item)
            , case onClose of
                Just onCloseClick ->
                    el [ pointer, Element.Events.onClick (onCloseClick item), Font.color iconColor ] <| Hatchinq.Theme.icon "cancel"

                Nothing ->
                    none
            ]
        ]
