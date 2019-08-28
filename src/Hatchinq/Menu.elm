module Hatchinq.Menu exposing (MenuItem(..), Message(..), State, View, configure, init, subscriptions, update)

import Browser.Events
import Element exposing (Element, Length, centerY, column, el, fill, height, htmlAttribute, minimum, mouseOver, paddingXY, pointer, px, scale, spacing, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Hatchinq.Attribute exposing (Attribute, toElement)
import Hatchinq.Divider as Divider exposing (withColor)
import Hatchinq.Theme as Theme exposing (Theme, icon, textWithEllipsis)
import Hatchinq.Util exposing (onClickPropagation, outsideTarget)
import Html.Attributes as Attr
import Json.Decode as Decode
import Task


-- TYPES


{-| -}
type alias Config msg =
    { theme : Theme
    , lift : Message msg -> msg
    }


type alias State =
    { isOpen : Bool
    }


subscriptions : State -> (Message msg -> msg) -> Sub msg
subscriptions state lift =
    if state.isOpen then
        Browser.Events.onMouseDown (outsideTarget "menu"
                                    |> Decode.andThen
                                     (\isOutside->
                                      if isOutside
                                       then Decode.succeed (CloseMenu Nothing)
                                        else Decode.fail "menu item clicked"))
        |> Sub.map lift
    else
        Sub.none


init : State
init =
    { isOpen = False
    }


-- MESSAGES


{-| -}
type Message msg
    = OpenMenu
    | CloseMenu (Maybe msg)


update : Message msg -> State -> ( State, Cmd msg )
update message state =
    case message of
        OpenMenu ->
            ( { state | isOpen = True }, Cmd.none )

        CloseMenu maybeAction ->
            ( { state | isOpen = False }
            , Maybe.map (\action -> Task.perform identity <| Task.succeed action) maybeAction
                |> Maybe.withDefault Cmd.none
            )



-- VIEW


type alias View msg =
    { items : List (MenuItem msg)
    , state : State
    }


type MenuItem msg
    = TextItem String msg
    | IconItem String String msg
    | DividerItem


{-| -}
configure : Config msg -> (List (Attribute msg) -> View msg -> Element msg)
configure config =
    view config


view : Config msg -> List (Attribute msg) -> View msg -> Element msg
view config attributes data =
    let
        elementAttributes =
            toElement attributes
    in
    menuBody config elementAttributes data


menuBody : Config msg -> List (Element.Attribute msg) -> View msg -> Element msg
menuBody config attributes { items, state } =
    let
        standardBodyAttributes =
            [ Element.htmlAttribute <| Attr.id "menu"
            , Border.rounded 4
            , Background.color Theme.white
            , paddingXY 0 8
            , width (minimum 112 fill)
            , htmlAttribute <| Attr.style "box-shadow" "0 5px 5px -3px rgba(0,0,0,.2), 0 8px 10px 1px rgba(0,0,0,.14), 0 3px 14px 2px rgba(0,0,0,.12)"
            ]

        bodyAttributes =
            case state.isOpen of
                True ->
                    htmlAttribute (Attr.style "opacity" "1")
                        :: scale 1
                        :: standardBodyAttributes

                False ->
                    htmlAttribute (Attr.style "opacity" "0")
                        :: scale 0
                        :: standardBodyAttributes
    in
    el
        (bodyAttributes ++ attributes)
        (column [] (List.map (\value -> menuItem config value) items))


menuItem : Config msg -> MenuItem msg -> Element msg
menuItem { theme, lift } item =
    let
        itemAttributes =
            [ pointer
            , Font.family [ theme.font.main ]
            , Font.size theme.font.smallSize
            , Font.regular
            , Font.color theme.colors.gray.dark
            , width fill
            , mouseOver [ Background.color theme.colors.gray.lighter ]
            , paddingXY 16 0
            , height (px 48)
            ]

        divider =
            Divider.configure { theme = theme }
    in
    case item of
        TextItem label action ->
            textItem itemAttributes label (lift <| CloseMenu (Just action))

        DividerItem ->
            divider [ withColor theme.colors.gray.lighter ]

        IconItem icon label action ->
            iconItem itemAttributes icon label (lift <| CloseMenu (Just action))


textItem : List (Element.Attribute msg) -> String -> msg -> Element msg
textItem attr label action =
    Element.el (attr ++ [onClickPropagation True action])
        (el [ centerY ] (textWithEllipsis <| label))


iconItem : List (Element.Attribute msg) -> String -> String -> msg -> Element msg
iconItem attr iconSource label action =
    Element.el (attr ++ [onClickPropagation True action])
        (Element.row [ spacing 16, centerY ]
            [ column [] [ icon iconSource ]
            , column [] [ textWithEllipsis <| label ]
            ]
        )