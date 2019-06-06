module Hatchinq.Card exposing (Config, Layout(..), Message(..), State, Title, View, configure, expandable, init, layout, update)

{-|


# Exposed

@docs Config, Layout, Message, State, Thumbnail, Title, View, configure, expandable, init, layout, update

-}

import Element exposing (Element, centerX, centerY, column, el, fill, height, htmlAttribute, maximum, minimum, padding, paddingEach, paddingXY, px, rgba255, row, shrink, spacing, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Hatchinq.Attribute as Attribute exposing (Attribute, custom, toElement, toInternalConfig)
import Hatchinq.Button as Button
import Hatchinq.Common exposing (Thumbnail(..), roundImage)
import Hatchinq.IconButton as IconButton
import Hatchinq.Theme as Theme exposing (Theme, icon, textWithEllipsis)
import Hatchinq.Util exposing (takeFirstNLines)
import Html
import Html.Attributes



-- TYPES


{-| -}
type alias Config msg =
    { theme : Theme
    , lift : Message msg -> msg
    }


type alias InternalConfig =
    { expandable : Bool
    , layout : Layout
    }


{-| -}
type alias State =
    { contentExpanded : Bool
    }


{-| -}
type Layout
    = MediaTop
    | MediaCenter


{-| -}
type alias Title =
    { head : String
    , subHead : Maybe String
    }


{-| -}
init : State
init =
    { contentExpanded = False
    }


{-| -}
expandable : Attribute InternalConfig
expandable =
    custom (\v -> { v | expandable = True })


{-| -}
layout : Layout -> Attribute InternalConfig
layout msg =
    custom (\v -> { v | layout = msg })



-- MESSAGES


{-| -}
type Message msg
    = ToggleExpanded



-- UPDATE


{-| -}
update : (Message msg -> msg) -> Message msg -> State -> ( State, Cmd msg )
update _ message state =
    case message of
        ToggleExpanded ->
            ( { state | contentExpanded = not state.contentExpanded }, Cmd.none )



-- VIEW


{-| -}
type alias View msg =
    { media : Element msg
    , titles : Title
    , thumbnail : Thumbnail
    , content : Element msg
    , actions : List ( String, msg )
    , state : State
    }


{-| -}
configure : Config msg -> (List (Attribute InternalConfig) -> View msg -> Element msg)
configure config =
    view config


view : Config msg -> List (Attribute InternalConfig) -> View msg -> Element msg
view { theme, lift } attributes { media, titles, thumbnail, content, actions, state } =
    let
        defaultWidth =
            344

        defaultHeight =
            382

        defaultInternalConfig =
            { expandable = False
            , layout = MediaCenter
            }

        internalConfig =
            toInternalConfig attributes defaultInternalConfig

        imageOrIcon =
            case thumbnail of
                Image url ->
                    roundImage url

                Icon url ->
                    el
                        [ width (px 40)
                        , height (px 40)
                        , paddingXY 26 0
                        ]
                        (el [ centerX, centerY ] (icon url))

        headers =
            column
                [ paddingEach { top = 2, bottom = 2, left = 0, right = 8 }
                , Font.family [ theme.font.main ]
                , Font.size theme.font.defaultSize
                , Font.bold
                , width fill
                , height shrink
                , centerY
                , htmlAttribute <| Html.Attributes.style "display" "inline-block"
                , htmlAttribute <| Html.Attributes.style "overflow" "hidden"
                , htmlAttribute <| Html.Attributes.style "text-overflow" "ellipsis"
                ]
                [ Element.html <|
                    Html.text
                        (if titles.subHead == Nothing then
                            takeFirstNLines titles.head 2

                         else
                            takeFirstNLines titles.head 1
                        )
                , case titles.subHead of
                    Nothing ->
                        Element.none

                    Just subHead ->
                        el
                            [ Font.size theme.font.smallerSize
                            , Font.color theme.colors.gray.dark
                            , Font.semiBold
                            ]
                            (textWithEllipsis (takeFirstNLines subHead 2))
                ]

        mediaRow =
            row
                [ htmlAttribute <| Html.Attributes.style "overflow" "hidden"
                , width fill
                , height (fill |> maximum 194)
                , centerX
                , centerY
                ]
                [ media ]

        contentRow =
            row
                ([ width fill
                 , htmlAttribute <| Html.Attributes.style "overflow" "auto"
                 , htmlAttribute <| Html.Attributes.style "will-change" "max-height, opacity, padding"
                 , htmlAttribute <| Html.Attributes.style "transition" "transform 0.25s, opacity 0.25s, max-height 0.25s, padding 0.25s"
                 ]
                    ++ (if (internalConfig.expandable && state.contentExpanded) || not internalConfig.expandable then
                            [ htmlAttribute <| Html.Attributes.style "opacity" "1"
                            , height (shrink |> maximum 194)
                            ]

                        else
                            [ htmlAttribute <| Html.Attributes.style "opacity" "0"
                            , height (shrink |> maximum 0)
                            ]
                       )
                )
                [ el [ paddingEach { left = 0, right = 0, top = 8, bottom = 16 }, width fill, height fill ] content ]

        toggleContentButton =
            IconButton.configure { theme = theme }
                []
                { icon =
                    if state.contentExpanded then
                        "expand_less"

                    else
                        "expand_more"
                , onPress =
                    Just
                        (lift <| ToggleExpanded)
                }

        actionsButtons =
            List.map (\( text, action ) -> Button.configure { theme = theme } [ Button.text ] { label = text, onPress = Just action }) actions

        buttonsRow =
            if List.isEmpty actionsButtons then
                Element.none

            else
                row
                    [ width fill
                    , height shrink
                    , spacing 8
                    , padding 8
                    , htmlAttribute <| Html.Attributes.style "overflow-x" "auto"
                    ]
                    actionsButtons
    in
    column
        ([ centerX
         , centerY
         , Font.family [ theme.font.main ]
         , Font.size theme.font.defaultSize
         , width (fill |> maximum defaultWidth)
         , height shrink
         , htmlAttribute <| Html.Attributes.style "overflow" "hidden"
         , Border.color theme.colors.gray.lightest
         , Border.shadow { offset = ( 0, 0 ), size = 0, blur = 3, color = rgba255 140 140 140 0.58 }
         , Element.mouseOver [ Background.color theme.colors.gray.lightest, Border.shadow { offset = ( 0, 2 ), size = 2, blur = 4, color = rgba255 140 140 140 0.58 } ]
         , Border.rounded 4
         , htmlAttribute <| Html.Attributes.style "transition" "box-shadow 0.1s"
         ]
            ++ toElement attributes
        )
        (case internalConfig.layout of
            MediaCenter ->
                [ row
                    [ width (fill |> minimum 344)
                    , height (shrink |> maximum 80)
                    , paddingEach { left = 0, right = 0, top = 16, bottom = 16 }
                    , centerY
                    ]
                    [ imageOrIcon
                    , headers
                    ]
                , mediaRow
                , row
                    [ width fill
                    , paddingEach { left = 16, right = 16, top = 8, bottom = 0 }
                    ]
                    [ contentRow ]
                , row [ width fill ]
                    [ buttonsRow
                    , if internalConfig.expandable then
                        el [ paddingXY 8 8, Element.alignRight ] toggleContentButton

                      else
                        Element.none
                    ]
                ]

            MediaTop ->
                [ mediaRow
                , row
                    [ width (fill |> minimum 344)
                    , height (shrink |> maximum 80)
                    , paddingEach { left = 0, right = 0, top = 8, bottom = 8 }
                    , centerY
                    ]
                    [ imageOrIcon
                    , headers
                    , if internalConfig.expandable then
                        el [ paddingXY 8 0 ] toggleContentButton

                      else
                        Element.none
                    ]
                , row
                    [ width fill
                    , paddingEach { left = 16, right = 16, top = 0, bottom = 0 }
                    ]
                    [ contentRow ]
                , buttonsRow
                ]
        )
