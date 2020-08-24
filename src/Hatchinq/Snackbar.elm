module Hatchinq.Snackbar exposing
    ( Config, Content(..), Message, State, View, alert, configure, dismissible, maximumWidth, init, update
    , backgroundColor, fontColor, icon
    )

{-|


# Exposed

@docs Config, Content, Message, State, View, alert, configure, dismissible, maximumWidth, init, update

-}

import Color
import Delay exposing (TimeUnit(..))
import Element exposing (Color, Element, alignBottom, alignRight, centerX, column, el, fill, html, htmlAttribute, maximum, minimum, padding, paddingEach, paddingXY, px, row, shrink, text)
import Element.Background
import Element.Border
import Element.Font
import Hatchinq.Attribute as Attribute exposing (Attribute, custom, toInternalConfig, width)
import Hatchinq.Button as Button
import Hatchinq.IconButton as IconButton
import Hatchinq.Theme exposing (Theme, white)
import Hatchinq.Util exposing (takeFirstNLines)
import Html
import Html.Attributes
import Task



-- TYPES


{-| -}
type alias Config msg =
    { theme : Theme
    , lift : Message msg -> msg
    }


{-| -}
type Content msg
    = Plain String
    | WithAction String String msg


type alias InternalConfig =
    { dismissible : Bool
    , maximumWidth : Int
    , backgroundColor : Maybe Color
    , fontColor : Maybe Color
    , icon : Maybe String
    }


{-| -}
backgroundColor : Color -> Attribute InternalConfig
backgroundColor bc =
    custom (\v -> { v | backgroundColor = Just bc })


{-| -}
fontColor : Color -> Attribute InternalConfig
fontColor c =
    custom (\v -> { v | fontColor = Just c })


{-| -}
icon : String -> Attribute InternalConfig
icon i =
    custom (\v -> { v | icon = Just i })


{-| -}
dismissible : Attribute InternalConfig
dismissible =
    custom (\v -> { v | dismissible = True })


{-| -}
maximumWidth : Int -> Attribute InternalConfig
maximumWidth max =
    custom (\v -> { v | maximumWidth = max })


{-| -}
type alias State msg =
    { values : List (Content msg)
    , currentValue : Maybe (Content msg)
    , id : Int
    , isOpen : Bool
    }


{-| -}
init : State msg
init =
    { values = []
    , currentValue = Nothing
    , id = 0
    , isOpen = False
    }



-- MESSAGES


{-| -}
type Message msg
    = Close Int (Maybe msg)
    | Open (Maybe (Content msg))



-- UPDATE


{-| -}
update : (Message msg -> msg) -> Message msg -> State msg -> ( State msg, Cmd msg )
update lift message state =
    case message of
        Open maybeContent ->
            let
                values =
                    case maybeContent of
                        Just newContent ->
                            state.values ++ [ newContent ]

                        Nothing ->
                            state.values

                newId =
                    state.id + 1

                close =
                    \content -> Cmd.map lift <| Delay.after 5000 Millisecond (Close newId Nothing)
            in
            if state.isOpen then
                ( { state | values = values }, Cmd.none )

            else
                case values of
                    head :: otherValues ->
                        if maybeContent == Nothing || state.currentValue == Nothing then
                            ( { state | values = otherValues, currentValue = Just head, id = newId, isOpen = True }, close head )

                        else
                            ( { state | values = values }, Cmd.none )

                    _ ->
                        ( { state | currentValue = Nothing }, Cmd.none )

        Close id maybeAction ->
            if state.id == id then
                let
                    openAgainCmd =
                        Cmd.map lift <| Delay.after 500 Millisecond (Open Nothing)

                    actionCmd =
                        case maybeAction of
                            Just action ->
                                Task.perform identity <| Task.succeed action

                            Nothing ->
                                Cmd.none
                in
                ( { state | isOpen = False }, Cmd.batch [ openAgainCmd, actionCmd ] )

            else
                ( state, Cmd.none )


{-| -}
alert : (Message msg -> msg) -> Content msg -> Cmd msg
alert lift content =
    Cmd.map lift
        (Cmd.batch
            [ Task.perform identity <| Task.succeed (Open (Just content))
            ]
        )



-- VIEW


{-| -}
type alias View msg =
    { state : State msg
    }


{-| -}
configure : Config msg -> (List (Attribute InternalConfig) -> View msg -> Element msg)
configure config =
    view config


{-| -}
view : Config msg -> List (Attribute InternalConfig) -> View msg -> Element msg
view { theme, lift } attributes { state } =
    let
        defaultInternalConfig =
            { dismissible = False
            , maximumWidth = 344
            , backgroundColor = Nothing
            , fontColor = Nothing
            , icon = Nothing
            }

        internalConfig =
            toInternalConfig attributes defaultInternalConfig

        button =
            Button.configure { theme = theme }

        iconButton =
            IconButton.configure { theme = theme }

        getButton =
            \maybeValue ->
                case maybeValue of
                    Just value ->
                        Element.map lift
                            (row
                                [ paddingXY 8 0
                                , alignRight
                                ]
                                [ case value of
                                    Plain _ ->
                                        Element.none

                                    WithAction _ buttonText buttonMsg ->
                                        button [ Button.text, width (shrink |> maximum 120) ] { label = buttonText, onPress = Just (Close state.id (Just buttonMsg)) }
                                , dismissibleButton
                                ]
                            )

                    Nothing ->
                        Element.map lift
                            (row
                                [ paddingXY 8 0
                                , alignRight
                                ]
                                [ dismissibleButton ]
                            )

        bgColor =
            case internalConfig.backgroundColor of
                Just configColor ->
                    configColor

                _ ->
                    theme.colors.gray.dark

        ( textColor, iconElement ) =
            let
                iconPadding =
                    paddingEach { left = 16, top = 8, right = 0, bottom = 8 }

                iconHtmlAttributes =
                    [ Html.Attributes.class "material-icons", Html.Attributes.style "user-select" "none" ]
            in
            case internalConfig.fontColor of
                Just specifiedFontColor ->
                    case internalConfig.icon of
                        Just iconString ->
                            ( specifiedFontColor
                            , Element.el
                                [ iconPadding ]
                                (Html.i
                                    (iconHtmlAttributes ++ [ Html.Attributes.style "color" (Color.toCssString (Color.fromRgba (Element.toRgb specifiedFontColor))) ])
                                    [ Html.text iconString ]
                                    |> html
                                )
                            )

                        _ ->
                            ( specifiedFontColor, Element.none )

                _ ->
                    case internalConfig.icon of
                        Just iconString ->
                            ( white, Element.el [ iconPadding ] (Html.i iconHtmlAttributes [ Html.text iconString ] |> html) )

                        _ ->
                            ( white, Element.none )

        dismissibleButton =
            if internalConfig.dismissible then
                iconButton [ IconButton.withTextColor textColor, Attribute.width (px 36), Attribute.height (px 36) ] { icon = "close", onPress = Just (Close state.id Nothing) }

            else
                Element.none

        htmlAttributes =
            if state.isOpen then
                [ htmlAttribute <| Html.Attributes.style "opacity" "1"
                , htmlAttribute <| Html.Attributes.style "transform" "scale(1)"
                , htmlAttribute <| Html.Attributes.style "transition" "opacity .25s, transform .25s"
                ]

            else
                [ htmlAttribute <| Html.Attributes.style "opacity" "0"
                , htmlAttribute <| Html.Attributes.style "transform" "scale(0.8)"
                , htmlAttribute <| Html.Attributes.style "transition" "opacity .25s, transform .25s .25s"
                , htmlAttribute <| Html.Attributes.style "pointer-events" "none"
                ]
    in
    column
        [ htmlAttribute <| Html.Attributes.style "position" "fixed"
        , htmlAttribute <| Html.Attributes.style "width" "100%"
        , htmlAttribute <| Html.Attributes.style "height" "100%"
        , htmlAttribute <| Html.Attributes.style "top" "0"
        , htmlAttribute <| Html.Attributes.style "left" "0"
        , htmlAttribute <| Html.Attributes.style "pointer-events" "none"
        , padding 20
        ]
        [ row
            ([ Element.height (px (calculateHeight (getText state.currentValue)))
             , Element.width (shrink |> minimum 344 |> maximum internalConfig.maximumWidth)
             , alignBottom
             , centerX
             , Element.Background.color bgColor
             , Element.Border.rounded 4
             , Element.Border.shadow { offset = ( 0, 3 ), size = 0, blur = 3, color = Element.rgba255 140 140 140 0.74 }
             , Element.Font.color textColor
             , Element.Font.family [ theme.font.main ]
             , Element.Font.size theme.font.smallSize
             , htmlAttribute <| Html.Attributes.style "pointer-events" "all"
             ]
                ++ htmlAttributes
            )
            [ iconElement
            , el
                [ Element.width fill
                , htmlAttribute <| Html.Attributes.style "display" "inline-block"
                , htmlAttribute <| Html.Attributes.style "overflow" "hidden"
                , htmlAttribute <| Html.Attributes.style "text-overflow" "ellipsis"
                , htmlAttribute <| Html.Attributes.style "line-height" "1.6"
                , paddingEach { left = 16, right = 8, top = 0, bottom = 0 }
                ]
                (html <| Html.text (getText state.currentValue))
            , el [ Element.width shrink ] (getButton state.currentValue)
            ]
        ]


calculateHeight : String -> Int
calculateHeight text =
    if List.length (String.split "\n" text) > 1 then
        68

    else
        48


getText : Maybe (Content msg) -> String
getText maybeValue =
    case maybeValue of
        Just value ->
            case value of
                Plain text ->
                    takeFirstNLines text 2

                WithAction text _ _ ->
                    takeFirstNLines text 2

        Nothing ->
            ""
