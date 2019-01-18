module Hatchinq.TextField exposing (Config, Message, State, View, configure, init, update)

import Element exposing (Element, fill, focused, height, htmlAttribute, inFront, mouseOver, paddingEach, paddingXY, px, shrink, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Hatchinq.Attribute exposing (Attribute, toElement)
import Hatchinq.Theme as Theme exposing (Theme)
import Html.Attributes as Attr


type alias Config id msg =
    { theme : Theme
    , lift : Message id -> msg
    }


type Message id
    = Focus id
    | Blur id
    | Impossible String


type State id
    = InternalState (Maybe id)


type alias View id msg =
    { id : id
    , label : String
    , value : String
    , state : State id
    , onChange : Maybe (String -> msg)
    }


type TextFieldType
    = Outlined
    | Filled


configure : Config id msg -> (List (Attribute v) -> View id msg -> Element msg)
configure config =
    view config


init : State id
init =
    InternalState Nothing


update : Message id -> State id -> State id
update message (InternalState state) =
    InternalState
        (case message of
            Focus id ->
                Just id

            Blur id ->
                if state == Just id then
                    Nothing

                else
                    state

            _ ->
                state
        )


view : Config id msg -> List (Attribute v) -> View id msg -> Element msg
view { theme, lift } attributes { id, label, value, state, onChange } =
    let
        standardLabelAttributes =
            [ width fill, height shrink, Font.family [ Font.typeface "Avenir" ], Font.size 16, htmlAttribute (Attr.style "transition" "all .2s ease"), htmlAttribute (Attr.style "transform-origin" "top left"), htmlAttribute (Attr.style "pointer-events" "none"), htmlAttribute (Attr.style "-webkit-font-smoothing" "antialiased") ]

        isDisabled =
            onChange == Nothing

        labelAttributes =
            if state == InternalState (Just id) then
                paddingXY 16 8 :: Font.color theme.colors.secondary.color :: htmlAttribute (Attr.style "transform" "scale(0.75)") :: standardLabelAttributes

            else if value /= "" then
                paddingXY 16 8
                    :: Font.color
                        (if isDisabled then
                            theme.colors.gray.color

                         else
                            theme.colors.gray.dark
                        )
                    :: htmlAttribute (Attr.style "transform" "scale(0.75)")
                    :: standardLabelAttributes

            else
                paddingXY 12 20
                    :: Font.color
                        (if isDisabled then
                            theme.colors.gray.color

                         else
                            theme.colors.gray.dark
                        )
                    :: standardLabelAttributes

        labelElement =
            Element.el labelAttributes (Element.text label)
    in
    Input.text
        (Background.color theme.colors.gray.lighter
            :: Border.roundEach { topLeft = 4, topRight = 4, bottomLeft = 0, bottomRight = 0 }
            :: Border.widthEach { left = 0, top = 0, right = 0, bottom = 2 }
            :: Border.color
                (if isDisabled then
                    Theme.transparent

                 else
                    theme.colors.gray.color
                )
            :: Font.family [ theme.font.main ]
            :: Font.color
                (if isDisabled then
                    theme.colors.gray.withAlpha 0.48

                 else
                    Theme.black
                )
            :: Font.size 16
            :: width (px 280)
            :: height (px 56)
            :: paddingEach { left = 12, top = 20, right = 12, bottom = 4 }
            :: focused
                (Background.color theme.colors.gray.lighter
                    :: Border.color theme.colors.secondary.color
                    :: []
                )
            :: mouseOver
                (if isDisabled then
                    []

                 else
                    Background.color theme.colors.gray.light
                        :: Border.color theme.colors.gray.color
                        :: []
                )
            :: inFront labelElement
            :: (Events.onFocus <| lift <| Focus id)
            :: (Events.onLoseFocus <| lift <| Blur id)
            :: (Element.htmlAttribute <| Attr.disabled isDisabled)
            :: toElement attributes
        )
        { onChange = onChange |> Maybe.withDefault (lift << Impossible)
        , text = value
        , placeholder = Nothing
        , label = Input.labelHidden label
        }
