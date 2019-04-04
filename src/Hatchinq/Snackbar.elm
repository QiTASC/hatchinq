module Hatchinq.Snackbar exposing (Content(..), Message, State, View, alert, configure, dismissible, init, update, view)

import Delay exposing (TimeUnit(..))
import Element exposing (Element, px, shrink)
import Element.Background
import Element.Border
import Element.Font
import Hatchinq.Attribute as Attribute exposing (Attribute, custom, toInternalConfig)
import Hatchinq.Button as Button
import Hatchinq.IconButton as IconButton
import Hatchinq.Theme as Theme exposing (Theme, white)
import Html.Attributes
import Task



-- TYPES


type alias Config msg =
    { theme : Theme
    , lift : Message msg -> msg
    }


type Content msg
    = Plain String
    | WithAction String String msg


type alias InternalConfig =
    { dismissable : Bool
    }


dismissible : Attribute InternalConfig
dismissible =
    custom (\v -> { v | dismissable = True })


type alias State msg =
    { values : List (Content msg)
    , currentValue : Content msg
    , id : Int
    , isOpen : Bool
    }


init : State msg
init =
    { values = []
    , currentValue = Plain ""
    , id = 0
    , isOpen = False
    }



-- MESSAGES


type Message msg
    = Close Int (Maybe msg)
    | Open (Maybe (Content msg))



-- UPDATE


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
                        ( { state | values = otherValues, currentValue = head, id = newId, isOpen = True }, close head )

                    _ ->
                        ( state, Cmd.none )

        Close id maybeAction ->
            if state.id == id then
                let
                    openAgainCmd =
                        if List.isEmpty state.values then
                            Cmd.none

                        else
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


alert : (Message msg -> msg) -> Content msg -> Cmd msg
alert lift content =
    Cmd.map lift
        (Cmd.batch
            [ Task.perform identity <| Task.succeed (Open (Just content))
            ]
        )



-- VIEW


type alias View msg =
    { state : State msg
    }


configure : Config msg -> (List (Attribute InternalConfig) -> View msg -> Element msg)
configure config =
    view config


view : Config msg -> List (Attribute InternalConfig) -> View msg -> Element msg
view { theme, lift } attributes { state } =
    let
        defaultInternalConfig =
            { dismissable = False
            }

        internalConfig =
            toInternalConfig attributes defaultInternalConfig

        button =
            Button.configure { theme = theme }

        iconButton =
            IconButton.configure { theme = theme }

        getButton =
            \value ->
                Element.map lift
                    (Element.row
                        [ Element.paddingXY 8 0
                        , Element.alignRight
                        ]
                        [ case value of
                            Plain _ ->
                                Element.none

                            WithAction _ buttonText buttonMsg ->
                                button [ Button.text ] { label = buttonText, onPress = Just (Close state.id (Just buttonMsg)) }
                        , if internalConfig.dismissable then
                            iconButton [ IconButton.white, Attribute.width (px 36), Attribute.height (px 36) ] { icon = "close", onPress = Just (Close state.id Nothing) }

                          else
                            Element.none
                        ]
                    )

        htmlAttributes =
            if state.isOpen then
                [ Element.htmlAttribute <| Html.Attributes.style "opacity" "1"
                , Element.htmlAttribute <| Html.Attributes.style "transform" "scale(1)"
                , Element.htmlAttribute <| Html.Attributes.style "transition" "all .25s"
                ]

            else
                [ Element.htmlAttribute <| Html.Attributes.style "opacity" "0"
                , Element.htmlAttribute <| Html.Attributes.style "transform" "scale(0.8)"
                , Element.htmlAttribute <| Html.Attributes.style "transition" "opacity .25s, transform .25s .25s"
                ]
    in
    Element.column
        [ Element.htmlAttribute <| Html.Attributes.style "position" "fixed"
        , Element.htmlAttribute <| Html.Attributes.style "width" "100%"
        , Element.htmlAttribute <| Html.Attributes.style "height" "100%"
        , Element.htmlAttribute <| Html.Attributes.style "top" "0"
        , Element.htmlAttribute <| Html.Attributes.style "left" "0"
        , Element.htmlAttribute <| Html.Attributes.style "pointer-events" "none"
        , Element.padding 20
        ]
        [ Element.row
            ([ Element.height (px 48)
             , Element.width (Element.minimum 344 shrink)
             , Element.alignBottom
             , Element.centerX
             , Element.Background.color theme.colors.gray.dark
             , Element.Border.rounded 4
             , Element.Border.shadow { offset = ( 0, 3 ), size = 0, blur = 3, color = Element.rgba255 140 140 140 0.74 }
             , Element.Font.color white
             , Element.Font.family [ theme.font.main ]
             , Element.Font.size theme.font.smallSize
             , Element.htmlAttribute <| Html.Attributes.style "pointer-events" "all"
             ]
                ++ htmlAttributes
            )
            [ Element.el [ Element.paddingXY 16 0, Element.centerY ] (Element.text (getText state.currentValue))
            , getButton state.currentValue
            ]
        ]


getText : Content msg -> String
getText value =
    case value of
        Plain text ->
            text

        WithAction text _ _ ->
            text
