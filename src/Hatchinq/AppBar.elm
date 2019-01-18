module Hatchinq.AppBar exposing (Action, Config, Message, State, View, appBarHeight, configure, elevate, init, navigate, placeholder, update)

import Element exposing (Element, centerX, centerY, fill, height, mouseOver, padding, paddingXY, pointer, px, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Hatchinq.Attribute exposing (Attribute, custom, toElement, toInternalView)
import Hatchinq.Theme as Theme exposing (Theme, icon)
import Html.Attributes


appBarHeight =
    56


type alias Config =
    { theme : Theme
    }


type Message
    = NoMessage


type State
    = State InternalState


type alias InternalState =
    {}


type alias View msg =
    { title : Element msg
    , actions : List (Action msg)
    }


type alias Action msg =
    { icon : String
    , message : msg
    }


type alias InternalView msg =
    { navigation : Maybe msg
    , elevate : Bool
    }


elevate : Bool -> Attribute (InternalView msg)
elevate q =
    custom (\v -> { v | elevate = q })


navigate : msg -> Attribute (InternalView msg)
navigate message =
    custom (\v -> { v | navigation = Just message })


configure : Config -> (List (Attribute (InternalView msg)) -> View msg -> Element msg)
configure config =
    view config


init : State
init =
    State {}


update : Message -> State -> State
update message state =
    state


placeholder : List (Attribute (InternalView msg)) -> Element msg
placeholder source =
    Element.el
        (width fill
            :: height (px appBarHeight)
            :: toElement source
        )
        Element.none


view : Config -> List (Attribute (InternalView msg)) -> View msg -> Element msg
view { theme } source data =
    let
        attributes =
            toElement source

        internalView =
            toInternalView source <| { navigation = Nothing, elevate = False }
    in
    Element.el
        (Background.color theme.colors.primary.color
            :: Font.family [ Font.typeface "Avenir" ]
            :: Font.size 20
            :: Font.bold
            :: Font.color Theme.white
            :: width fill
            :: height (px appBarHeight)
            :: padding 8
            :: (if internalView.elevate then
                    Border.shadow { offset = ( 0, 4 ), size = 0, blur = 4, color = Element.rgba255 140 140 140 0.58 }

                else
                    Border.width 0
               )
            :: attributes
        )
        (Element.row [ width fill, Element.spacing 16 ]
            [ case internalView.navigation of
                Just message ->
                    iconButton theme "menu" message

                Nothing ->
                    Element.none

            -- todo add navigation
            , Element.el [ paddingXY 16 0 ] data.title
            , Element.row [ Element.alignRight, Element.spacing 8 ]
                (data.actions |> List.map (\a -> iconButton theme a.icon a.message))
            ]
        )


iconButton : Theme -> String -> msg -> Element msg
iconButton theme iconName message =
    Element.el
        [ Events.onClick message
        , Border.rounded 20
        , width (px 40)
        , height (px 40)
        , Element.htmlAttribute
            (Html.Attributes.class "button focusWhiteRipple")
        , mouseOver
            [ Background.color theme.colors.gray.light ]
        , pointer
        ]
        (Element.el [ centerX, centerY ] (icon iconName))
