module Hatchinq.TabBar exposing (Config, Message, State, TabButtons(..), View, configure, init, scrollable, update)

{-|


# Exposed

@docs Config, Message, State, TabButtons, View, configure, init, scrollable, update

-}

import Array
import Dict
import Element exposing (Element, fill, maximum, minimum, px, shrink)
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Hatchinq.Attribute exposing (Attribute, custom, toElement)
import Hatchinq.Theme exposing (Theme, icon, textWithEllipsis)
import Hatchinq.Util exposing (arrowLeftKeyCode, arrowRightKeyCode, enterKeyCode, keysDownAttribute)
import Html.Attributes
import Task



-- TYPES


{-| -}
type alias Config msg =
    { theme : Theme
    , lift : Message msg -> msg
    }


{-| -}
configure : Config msg -> (List (Attribute InternalConfig) -> View tab msg -> Element msg)
configure config =
    view config


type TabBarType
    = Fixed
    | Scrollable


{-| -}
type alias State =
    { focused : Int
    }


{-| -}
init : State
init =
    { focused = -1
    }


{-| -}
type TabButtons tab
    = TextOnly (List ( String, tab ))
    | IconOnly (List ( String, tab ))
    | IconAndText (List ( String, String, tab ))


type alias InternalConfig =
    { tabBarType : TabBarType
    }


{-| -}
scrollable : Attribute InternalConfig
scrollable =
    custom (\v -> { v | tabBarType = Scrollable })



-- MESSAGES


{-| -}
type Message msg
    = SelectTab Int msg
    | Focus Int
    | Blur



-- UPDATE


{-| -}
update : (Message msg -> msg) -> Message msg -> State -> ( State, Cmd msg )
update lift message state =
    case message of
        SelectTab index onSelect ->
            ( { state | focused = index }, Task.perform identity <| Task.succeed onSelect )

        Focus index ->
            ( { state | focused = index }, Cmd.none )

        Blur ->
            ( { state | focused = -1 }, Cmd.none )



-- VIEW


{-| -}
type alias View tab msg =
    { state : State
    , tabButtons : TabButtons tab
    , selectedTab : tab
    , onTabSelect : tab -> msg
    }


view : Config msg -> List (Attribute InternalConfig) -> View tab msg -> Element msg
view { lift, theme } attributes { state, tabButtons, selectedTab, onTabSelect } =
    let
        textContent =
            \text -> Element.el [ Element.width (minimum 90 fill), Font.center, Element.centerY, Element.paddingXY 16 0 ] (textWithEllipsis text)

        iconContent =
            \iconName -> Element.el [ Element.width (minimum 90 fill), Font.center, Element.centerY, Element.paddingXY 16 0 ] (icon iconName)

        iconAndTextContent =
            \iconName text ->
                Element.column
                    [ Element.width (minimum 90 fill)
                    , Font.center
                    , Element.centerY
                    , Element.paddingXY 16 0
                    ]
                    [ Element.el [ Element.width fill, Element.paddingXY 0 6 ] (icon iconName), textWithEllipsis text ]

        contentElement =
            \content index tab ->
                Element.el
                    ([ Element.width (maximum 360 shrink)
                     , Element.height fill
                     , Element.inFront
                        (Element.el [ Element.width fill, Element.height fill, Element.htmlAttribute <| Html.Attributes.class "ripple focusPrimaryRipple" ]
                            (Element.el
                                [ Element.width fill
                                , Element.height (px 2)
                                , Element.alignBottom
                                , Background.color theme.colors.primary.color
                                , Element.htmlAttribute <| Html.Attributes.style "transition" "all 0.25s"
                                , Element.htmlAttribute <| Html.Attributes.style "will-change" "transform"
                                , if tab == selectedTab then
                                    Element.htmlAttribute <| Html.Attributes.style "transform" "scaleY(1)"

                                  else
                                    Element.htmlAttribute <| Html.Attributes.style "transform" "scaleY(0)"
                                ]
                                Element.none
                            )
                        )
                     , Events.onClick (lift <| SelectTab index (onTabSelect tab))
                     ]
                        ++ (if tab == selectedTab then
                                [ Font.color theme.colors.primary.color
                                ]

                            else
                                [ Font.color theme.colors.gray.dark
                                ]
                           )
                        ++ (if index == state.focused then
                                [ Background.color theme.colors.primary.lighter ]

                            else
                                [ Element.mouseOver [ Background.color theme.colors.primary.lightest ] ]
                           )
                    )
                    content

        findSelectedTabIndex =
            \list ->
                List.head
                    (List.filterMap
                        (\( index, tab ) ->
                            if tab == selectedTab then
                                Just index

                            else
                                Nothing
                        )
                        list
                    )

        ( buttons, maybeFocusedTab, selectedTabIndex ) =
            case tabButtons of
                TextOnly textItems ->
                    ( List.indexedMap (\index ( text, tab ) -> contentElement (textContent text) index tab) textItems
                    , Maybe.map (\( _, tab ) -> tab) (Array.get state.focused (Array.fromList textItems))
                    , findSelectedTabIndex (List.indexedMap (\index ( _, tab ) -> ( index, tab )) textItems)
                    )

                IconOnly iconItems ->
                    ( List.indexedMap (\index ( iconName, tab ) -> contentElement (iconContent iconName) index tab) iconItems
                    , Maybe.map (\( _, tab ) -> tab) (Array.get state.focused (Array.fromList iconItems))
                    , findSelectedTabIndex (List.indexedMap (\index ( _, tab ) -> ( index, tab )) iconItems)
                    )

                IconAndText items ->
                    ( List.indexedMap (\index ( iconName, text, tab ) -> contentElement (iconAndTextContent iconName text) index tab) items
                    , Maybe.map (\( _, _, tab ) -> tab) (Array.get state.focused (Array.fromList items))
                    , findSelectedTabIndex (List.indexedMap (\index ( _, _, tab ) -> ( index, tab )) items)
                    )

        buttonsCount =
            List.length buttons

        keyDownAttributes =
            [ keysDownAttribute
                (Dict.fromList
                    ([ ( arrowLeftKeyCode
                       , lift
                            (Focus <|
                                if state.focused <= 1 then
                                    0

                                else
                                    state.focused - 1
                            )
                       )
                     , ( arrowRightKeyCode
                       , lift
                            (Focus <|
                                if state.focused >= (buttonsCount - 1) then
                                    buttonsCount - 1

                                else
                                    state.focused + 1
                            )
                       )
                     ]
                        ++ (case maybeFocusedTab of
                                Just focusedTab ->
                                    [ ( enterKeyCode, lift (SelectTab state.focused (onTabSelect focusedTab)) ) ]

                                Nothing ->
                                    []
                           )
                    )
                )
            ]

        defaultElementAttributes =
            [ Element.width shrink ]
                ++ (case tabButtons of
                        IconAndText _ ->
                            [ Element.height (px 72) ]

                        _ ->
                            [ Element.height (px 48) ]
                   )

        elementAttributes =
            toElement attributes
    in
    Element.row
        ([ Element.width fill
         , Font.family [ theme.font.main ]
         , Font.size 14
         , Font.bold
         , Element.htmlAttribute <| Html.Attributes.style "text-transform" "uppercase"
         , Element.htmlAttribute <| Html.Attributes.attribute "tabindex" "0"
         , Events.onFocus
            (lift <|
                Focus (Maybe.withDefault 0 selectedTabIndex)
            )
         , Events.onLoseFocus (lift <| Blur)
         ]
            ++ keyDownAttributes
            ++ defaultElementAttributes
            ++ elementAttributes
        )
        buttons
