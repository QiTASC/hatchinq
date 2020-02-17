module Hatchinq.SidePanel exposing (Config, SidePanelOrientation(..), State, View, configure, init, minWidth, subscriptions)

{-|


# Exposed

@docs Config, SidePanelOrientation, State, View, configure, init, minWidth, subscriptions

-}

import Array
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events
import Dict exposing (Dict)
import Element exposing (Element, Length, alignTop, centerX, centerY, column, fill, height, html, htmlAttribute, paddingXY, pointer, px, row, shrink, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Hatchinq.Attribute exposing (Attribute, custom, toElement, toInternalConfig)
import Hatchinq.MouseDecoder exposing (MousePosition, positionDecoder)
import Hatchinq.Theme as Theme exposing (Theme)
import Html exposing (i)
import Html.Attributes exposing (class, style)
import Html.Events
import Json.Decode as Decode
import Task


{-| -}
type alias Config msg =
    { theme : Theme
    , lift : State -> msg
    , orientation : SidePanelOrientation
    , resizeMaxWidthFraction : Maybe Float
    , initialWidthFraction : Float
    }


{-| -}
type SidePanelOrientation
    = LeftHand
    | RightHand


type alias WindowSize =
    { width : Int
    , height : Int
    }


{-| -}
type alias State =
    { openedContainerId : Int
    , containerWidth : Int
    , containerWidths : Dict Int Int
    , beingDragged : Bool
    , windowSize : WindowSize
    }


{-| -}
type alias View msg =
    { buttons : List (SidePanelButton msg)
    , state : State
    , topPageOffset : Int
    }


type alias InternalConfig =
    { minWidth : Maybe Int
    , elevate : Bool
    }


{-| -}
minWidth : Int -> Attribute InternalConfig
minWidth m =
    custom (\v -> { v | minWidth = Just m })


elevate : Attribute InternalConfig
elevate =
    custom (\v -> { v | elevate = True })


type alias SidePanelButton msg =
    { id : Maybe String, icon : String, title : String, containerContent : () -> Element msg }


initialOpenPanelWidth defaultWidthFraction maxWidthFraction windowSize =
    round <| min defaultWidthFraction (Maybe.withDefault defaultWidthFraction maxWidthFraction) * toFloat windowSize.width


closedSidePanelWidth =
    33


dragHandleWidth =
    8


{-| -}
subscriptions : Config msg -> State -> Sub msg
subscriptions config state =
    case ( state.beingDragged, config.resizeMaxWidthFraction ) of
        ( True, Just maxWidthFraction ) ->
            Sub.batch
                [ positionDecoder
                    |> Decode.map (\pos -> stateFromDrag config maxWidthFraction state pos)
                    |> Decode.map config.lift
                    |> Browser.Events.onMouseMove
                , Decode.succeed { state | beingDragged = False }
                    |> Decode.map config.lift
                    |> Browser.Events.onMouseUp
                ]

        _ ->
            Sub.batch
                [ Browser.Events.onResize (\width height -> config.lift { state | windowSize = WindowSize width height }) ]


stateFromDrag : Config msg -> Float -> State -> MousePosition -> State
stateFromDrag config maxWidthFraction state pos =
    let
        maxWidth =
            round <| maxWidthFraction * (toFloat <| state.windowSize.width)

        newWidth =
            if config.orientation == LeftHand then
                pos.x - closedSidePanelWidth

            else
                state.windowSize.width - closedSidePanelWidth - pos.x

        clampedWidth =
            if newWidth < 0 then
                0

            else if newWidth > maxWidth then
                maxWidth

            else
                newWidth
    in
    { state
        | containerWidth = clampedWidth
    }


{-| -}
init : Int -> Config msg -> ( State, Cmd msg )
init openedContainerId config =
    let
        state =
            State -1 0 Dict.empty False (WindowSize 0 0)
    in
    ( state
    , Dom.getViewport
        |> Task.perform (\v -> initState state openedContainerId config v)
        |> Cmd.map config.lift
    )


initState : State -> Int -> Config msg -> Viewport -> State
initState state openedContainerId { lift, resizeMaxWidthFraction, initialWidthFraction } viewport =
    let
        windowSize =
            WindowSize (round viewport.viewport.width) (round viewport.viewport.height)

        defaultContainerWidth =
            initialOpenPanelWidth initialWidthFraction resizeMaxWidthFraction windowSize

        ( containerWidth, containerWidths ) =
            if openedContainerId /= -1 then
                ( defaultContainerWidth, Dict.fromList [ ( openedContainerId, defaultContainerWidth ) ] )

            else
                ( 0, Dict.empty )
    in
    { state | windowSize = windowSize, openedContainerId = openedContainerId, containerWidth = containerWidth, containerWidths = containerWidths }


{-| -}
configure : Config msg -> (List (Attribute InternalConfig) -> View msg -> Element msg)
configure config =
    view config


view : Config msg -> List (Attribute InternalConfig) -> View msg -> Element msg
view config source v =
    let
        { theme, lift, orientation } =
            config

        defaultInternalConfig =
            { minWidth = Nothing
            , elevate = False
            }

        internalConfig =
            toInternalConfig source defaultInternalConfig

        sideBarButtons =
            Element.map lift <| sidebarButtons config v

        widthPx =
            if v.state.openedContainerId /= -1 then
                case internalConfig.minWidth of
                    Just m ->
                        max m v.state.containerWidth

                    Nothing ->
                        v.state.containerWidth

            else
                0

        container =
            Element.el
                [ width (px widthPx)
                , height <| px <| v.state.windowSize.height - v.topPageOffset
                , Element.scrollbars
                ]
                (containerView v)

        dragHandleElement =
            Element.map lift <| dragHandle config v.state

        containerShadow =
            if v.state.openedContainerId /= -1 && internalConfig.elevate then
                [ Border.shadow { offset = ( 1 * offsetMultiplier config, 0 ), size = 1, blur = 2, color = config.theme.colors.gray.light } ]

            else
                []
    in
    Element.row
        (height fill
            :: width shrink
            :: Font.family [ config.theme.font.main ]
            :: Font.size 16
            :: containerShadow
            ++ toElement source
        )
        (if orientation == LeftHand then
            [ sideBarButtons
            , container
            , dragHandleElement
            ]

         else
            [ dragHandleElement
            , container
            , sideBarButtons
            ]
        )


containerView : View msg -> Element msg
containerView v =
    if v.state.openedContainerId == -1 then
        Element.none

    else
        Array.fromList v.buttons
            |> Array.get v.state.openedContainerId
            |> Maybe.map (\btn -> btn.containerContent ())
            |> Maybe.withDefault Element.none


sidebarButtons : Config msg -> View msg -> Element State
sidebarButtons config v =
    let
        buttons =
            List.indexedMap (\index button -> toSidePanelButton index button v.state config) v.buttons

        border =
            if config.orientation == LeftHand then
                Border.widthEach { bottom = 0, top = 0, left = 0, right = 1 }

            else
                Border.widthEach { bottom = 0, top = 0, left = 1, right = 0 }

        sideBarHeight =
            v.state.windowSize.height - v.topPageOffset
    in
    column
        [ width (px closedSidePanelWidth)
        , height <| px sideBarHeight
        , Background.color config.theme.colors.gray.lightest
        , Font.color config.theme.colors.gray.dark
        , alignTop
        , border
        , Border.color config.theme.colors.gray.light
        ]
        [ row
            [ pointer
            , htmlAttribute <| style "user-select" "none"
            , width <| Element.maximum sideBarHeight shrink
            , htmlAttribute <| style "transform-origin" "0% 100%"
            , htmlAttribute <| style "transform" "translateY(-100%) rotate(90deg)"
            ]
            buttons
        ]


offsetMultiplier : Config msg -> Float
offsetMultiplier config =
    if config.orientation == LeftHand then
        1

    else
        -1


toSidePanelButton : Int -> SidePanelButton msg -> State -> Config msg -> Element State
toSidePanelButton index btn state config =
    let
        backgroundColor =
            if state.openedContainerId == index then
                config.theme.colors.gray.light

            else
                Theme.transparent
    in
    row
        ([ Element.paddingEach { top = 4, right = 12, bottom = 4, left = 4 }
         , Events.onClick <| stateFromSelectionChanged index state config
         , Background.color backgroundColor
         , if config.orientation == LeftHand then
            Element.rotate pi

           else
            Element.rotate 0
         ]
            ++ Maybe.withDefault [] (Maybe.map (\id -> [ Element.htmlAttribute <| Html.Attributes.id id ]) btn.id)
        )
        [ Element.el
            [ centerX
            , centerY
            , Element.rotate <| (pi / 2) * offsetMultiplier config
            , paddingXY 4 0
            , Font.color config.theme.colors.primary.light
            ]
            (i [ class "material-icons" ] [ Html.text btn.icon ] |> html)
        , text btn.title
        ]


stateFromSelectionChanged : Int -> State -> Config msg -> State
stateFromSelectionChanged index state { initialWidthFraction, resizeMaxWidthFraction } =
    let
        newContainerWidths =
            Dict.insert state.openedContainerId state.containerWidth state.containerWidths

        containerWidth =
            Maybe.withDefault (initialOpenPanelWidth initialWidthFraction resizeMaxWidthFraction state.windowSize) (Dict.get index state.containerWidths)
    in
    if index == state.openedContainerId then
        { state
            | openedContainerId = -1
            , containerWidth = 0
            , containerWidths = newContainerWidths
        }

    else
        { state
            | openedContainerId = index
            , containerWidth = containerWidth
            , containerWidths = newContainerWidths
        }


dragHandle : Config msg -> State -> Element State
dragHandle config state =
    let
        overlayWidth =
            100

        handleOverlay =
            if config.resizeMaxWidthFraction /= Nothing then
                Element.el
                    [ htmlAttribute <| onMouseDownNoBubble { state | beingDragged = True }
                    , height fill
                    , width <| px <| dragHandleWidth
                    , htmlAttribute <| style "cursor" "col-resize"
                    , htmlAttribute <| style "z-index" "900"
                    , Element.inFront activeDragOverlay
                    , centerX
                    ]
                    Element.none

            else
                Element.none

        activeDragOverlay =
            if state.beingDragged then
                Element.el
                    [ width <| px overlayWidth
                    , Element.moveLeft <| overlayWidth / 2
                    , height fill
                    , htmlAttribute <| style "cursor" "col-resize"
                    , htmlAttribute <| style "z-index" "1000"
                    ]
                    Element.none

            else
                Element.none
    in
    if state.openedContainerId == -1 then
        Element.none

    else
        Element.el
            [ Background.color config.theme.colors.gray.light
            , width <| px 1
            , height fill
            , Element.centerX
            , Element.inFront handleOverlay
            ]
            Element.none


onMouseDownNoBubble : msg -> Html.Attribute msg
onMouseDownNoBubble msg =
    Html.Events.custom "mousedown"
        (Decode.succeed { message = msg, stopPropagation = True, preventDefault = True })
