module Hatchinq.Chip exposing (coloring, configure, icon, label, maxWidth, view, withError)

import Element exposing (Color, Element, alignBottom, centerY, column, el, fill, maximum, moveDown, none, padding, pointer, rgb255, row, shrink, spacing, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font exposing (size)
import Hatchinq.Attribute exposing (Attribute, custom, toInternalConfig)
import Hatchinq.Theme exposing (Theme, textWithEllipsis)



-- TYPES


{-| -}
type alias Config =
    { theme : Theme }


type alias InternalConfig item =
    { label : Maybe String
    , error : Bool
    , coloring : Maybe (item -> Color)
    , icon : Maybe String
    , maxWidth : Maybe Int
    }


{-| -}
label : String -> Attribute (InternalConfig item)
label text =
    custom (\v -> { v | label = Just text })


{-| -}
withError : Bool -> Attribute (InternalConfig item)
withError error =
    custom (\v -> { v | error = error })


{-| -}
coloring : (item -> Element.Color) -> Attribute (InternalConfig item)
coloring coloring_ =
    custom (\v -> { v | coloring = Just coloring_ })


{-| -}
icon : String -> Attribute (InternalConfig item)
icon name =
    custom (\v -> { v | icon = Just name })


{-| -}
maxWidth : Int -> Attribute (InternalConfig item)
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
configure : Config -> (List (Attribute (InternalConfig item)) -> View item msg -> Element msg)
configure config =
    view config


view : Config -> List (Attribute (InternalConfig item)) -> View item msg -> Element msg
view { theme } attributes { item, toString, onClick, onClose } =
    let
        defaultConfig =
            { label = Nothing
            , error = False
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
        ([ Border.rounded 24, Background.color backgroundColor, padding 8, spacing 4, alignBottom ] ++ (Maybe.withDefault [] <| Maybe.map (\click -> [ Element.Events.onClick (click item) ]) onClick))
        [ Maybe.withDefault none <| Maybe.map (\content -> el [ Font.color theme.colors.gray.dark, size 12 ] <| textWithEllipsis content) internalConfig.label
        , row
            [ spacing 8, centerY ]
            [ Maybe.withDefault none <| Maybe.map (\ic -> el [ Font.color iconColor ] <| Hatchinq.Theme.icon ic) internalConfig.icon
            , el [ textWidth, moveDown 2 ] <| textWithEllipsis (toString item)
            , case onClose of
                Just onCloseClick ->
                    el [ pointer, Element.Events.onClick (onCloseClick item), Font.color iconColor ] <| Hatchinq.Theme.icon "cancel"

                Nothing ->
                    none
            ]
        ]
