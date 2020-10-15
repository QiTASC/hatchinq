module Hatchinq.TextField exposing (Config, Message, State, View, configure, init, multiline, password, update, withError, onFocus, onLoseFocus)

{-|


# Exposed

@docs Config, Message, State, View, configure, init, multiline, onFocus, onLoseFocus, password, update, withError

-}

import Element exposing (Element, fill, height, htmlAttribute, inFront, mouseOver, paddingEach, paddingXY, px, shrink, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Hatchinq.Attribute exposing (Attribute, custom, toHeight, toId, toInternalConfig, toWidth)
import Hatchinq.Theme as Theme exposing (Theme, textWithEllipsis, transparent)
import Html.Attributes as Attr
import Html.Events
import Json.Decode as Json
import Task



-- TYPES


{-| -}
type alias Config id msg =
    { theme : Theme
    , lift : Message msg id -> msg
    }


{-| -}
type State id
    = InternalState (Maybe id)


type alias InternalConfig msg=
    { multiline : Bool
    , password : Bool
    , errorFunction : Maybe { default : String, error : Maybe String }
    , onFocus : Maybe msg
    , onLoseFocus: Maybe msg
    }


type TextFieldType
    = Outlined
    | Filled


{-| -}
configure : Config id msg -> (List (Attribute (InternalConfig msg)) -> View id msg -> Element msg)
configure config =
    view config


{-| -}
multiline : Attribute (InternalConfig msg)
multiline =
    custom (\v -> { v | multiline = True })


{-| -}
password : Attribute (InternalConfig msg)
password =
    custom (\v -> { v | password = True })


{-| -}
withError : { default : String, error : Maybe String } -> Attribute (InternalConfig msg)
withError errorFunction =
    custom (\v -> { v | errorFunction = Just errorFunction })

{-| -}
onFocus : msg -> Attribute (InternalConfig msg)
onFocus msg =
    custom (\v -> { v | onFocus = Just msg })

{-| -}
onLoseFocus : msg -> Attribute (InternalConfig msg)
onLoseFocus msg =
    custom (\v -> { v | onLoseFocus = Just msg })

{-| -}
init : State id
init =
    InternalState Nothing



-- MESSAGES


{-| -}
type Message msg id
    = Focus id (Maybe msg)
    | Blur id (Maybe msg)
    | Impossible String



-- UPDATE


{-| -}
update : Message msg id -> State id -> (State id, Cmd msg)
update message (InternalState state) =

        (case message of
            Focus id Nothing ->
                (InternalState <| Just id, Cmd.none)

            Focus id (Just focusMsg) ->
                (InternalState <| Just id, Task.succeed focusMsg |> Task.perform identity)

            Blur id maybeLoseFocusMsg ->
                let
                    cmd =
                        Maybe.withDefault Cmd.none <| Maybe.map (\loseFocusMsg -> Task.succeed loseFocusMsg |> Task.perform identity) maybeLoseFocusMsg
                in
                if state == Just id then
                    (InternalState Nothing, cmd)

                else
                    (InternalState state, cmd)

            _ ->
                (InternalState state, Cmd.none)
        )



-- VIEW


{-| -}
type alias View id msg =
    { id : id
    , label : String
    , value : String
    , state : State id
    , onChange : Maybe (String -> msg)
    , onKeyDown : Maybe (Json.Decoder msg)
    }


view : Config id msg -> List (Attribute (InternalConfig msg)) -> View id msg -> Element msg
view { theme, lift } attributes { id, label, value, state, onChange, onKeyDown } =
    let
        defaultInternalConfig =
            { multiline = False
            , password = False
            , errorFunction = Nothing
            , onFocus = Nothing
            , onLoseFocus = Nothing
            }

        internalConfig =
            toInternalConfig attributes defaultInternalConfig

        errorColor =
            Element.rgb255 244 67 54

        colorIfError defaultColor =
            case internalConfig.errorFunction of
                Just errorFunction ->
                    if errorFunction.error /= Nothing then
                        errorColor

                    else
                        defaultColor

                _ ->
                    defaultColor

        standardLabelAttributes =
            [ width fill
            , height shrink
            , Font.family [ Font.typeface "Avenir" ]
            , Font.size 16
            , htmlAttribute (Attr.style "transition" "all .2s ease")
            , htmlAttribute (Attr.style "transform-origin" "top left")
            , htmlAttribute (Attr.style "pointer-events" "none")
            , htmlAttribute (Attr.style "-webkit-font-smoothing" "antialiased")
            ]

        isDisabled =
            onChange == Nothing

        focusedAttributes =
            if not isDisabled && state == InternalState (Just id) then
                [ Background.color theme.colors.gray.lighter
                , Border.color <| colorIfError theme.colors.secondary.color
                ]

            else
                []

        labelAttributes =
            if state == InternalState (Just id) then
                paddingXY 16 8 :: Font.color (colorIfError theme.colors.secondary.color) :: htmlAttribute (Attr.style "transform" "scale(0.75)") :: standardLabelAttributes

            else if value /= "" then
                paddingXY 16 8
                    :: Font.color
                        (if isDisabled then
                            theme.colors.gray.color

                         else
                            colorIfError theme.colors.gray.dark
                        )
                    :: htmlAttribute (Attr.style "transform" "scale(0.75)")
                    :: standardLabelAttributes

            else
                paddingXY 12 20
                    :: Font.color
                        (if isDisabled then
                            theme.colors.gray.color

                         else
                            colorIfError theme.colors.gray.dark
                        )
                    :: standardLabelAttributes

        labelElement =
            Element.el labelAttributes (Element.text label)

        passwordAttribute =
            if internalConfig.password then
                [ htmlAttribute <| Attr.type_ "password" ]

            else
                []

        idAttribute =
            case toId attributes of
                Just stringId ->
                    [ htmlAttribute <| Attr.id stringId ]
                Nothing ->
                    []

        inputAttributes =
            [ Events.onFocus <| lift <| Focus id internalConfig.onFocus
            , Events.onLoseFocus <| lift <| Blur id internalConfig.onLoseFocus
            , Background.color transparent
            , Element.scrollbarY
            , Border.width 0
            , htmlAttribute <| Attr.disabled isDisabled
            , paddingEach
                { left = 12
                , top =
                    if internalConfig.multiline then
                        0

                    else
                        6
                , right = 12
                , bottom = 4
                }
            ]
                ++ idAttribute
                ++ passwordAttribute
                ++ Maybe.withDefault [] (Maybe.map (\decoder -> [ Element.htmlAttribute <| Html.Events.on "keydown" decoder ]) onKeyDown)
    in
    Element.column
        [ width <| Maybe.withDefault (px 280) (toWidth attributes) ]
        [ Element.column
            ([ Background.color theme.colors.gray.lighter
             , Border.roundEach { topLeft = 4, topRight = 4, bottomLeft = 0, bottomRight = 0 }
             , Border.widthEach { left = 0, top = 0, right = 0, bottom = 2 }
             , Border.color
                (if isDisabled then
                    Theme.transparent

                 else
                    colorIfError theme.colors.gray.color
                )
             , Font.family [ theme.font.main ]
             , Font.color
                (if isDisabled then
                    theme.colors.gray.withAlpha 0.48

                 else
                    Theme.black
                )
             , Font.size 16
             , htmlAttribute <| Attr.style "word-break" "break-word"
             , width fill
             , mouseOver
                (if isDisabled || state == InternalState (Just id) then
                    []

                 else
                    Background.color theme.colors.gray.light
                        :: (Border.color <| colorIfError theme.colors.gray.color)
                        :: []
                )
             , Element.htmlAttribute <| Attr.disabled isDisabled
             , paddingEach
                { left = 0
                , top = 0
                , right = 0
                , bottom =
                    if internalConfig.multiline then
                        8

                    else
                        0
                }
             ]
                ++ focusedAttributes
            )
            [ Element.el [ height (px 16), inFront labelElement ] Element.none
            , if internalConfig.multiline then
                Element.el [ height (Maybe.withDefault fill (toHeight attributes)), width fill ] <|
                    Input.multiline
                        ([ htmlAttribute <| Attr.attribute "rows" "1", height (Maybe.withDefault fill (toHeight attributes)) ] ++ inputAttributes)
                        { onChange = onChange |> Maybe.withDefault (lift << Impossible)
                        , text = value
                        , placeholder = Nothing
                        , label = Input.labelAbove [] Element.none
                        , spellcheck = False
                        }

              else
                Input.text
                    (inputAttributes ++ [ height (px 32) ])
                    { onChange = onChange |> Maybe.withDefault (lift << Impossible)
                    , text = value
                    , placeholder = Nothing
                    , label = Input.labelHidden label
                    }
            ]
        , case internalConfig.errorFunction of
            Just errorFunction ->
                Element.el [ Font.size 12, height <| px 14, paddingEach { left = 4, top = 4, right = 0, bottom = 0 }, width <| Maybe.withDefault (px 280) (toWidth attributes) ] <|
                    case errorFunction.error of
                        Just error ->
                            Element.el [ Font.color errorColor ] <| textWithEllipsis error

                        Nothing ->
                            Element.el [ Font.color theme.colors.gray.dark ] <| textWithEllipsis errorFunction.default

            Nothing ->
                Element.none
        ]
