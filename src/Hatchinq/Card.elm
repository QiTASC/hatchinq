module Hatchinq.Card exposing
    ( Config, Layout(..), Message(..), State, Thumbnail(..), View, configure, expandable, init, layout, update
    , Title
    )

{-|


# Exposed

@docs Config, Layout, Message, State, Thumbnail, View, configure, expandable, init, layout, update

-}

import Element exposing (Element, centerY, fill, px, shrink)
import Element.Background as Background
import Element.Border
import Element.Font as Font
import Hatchinq.Attribute as Attribute exposing (Attribute, custom, toElement, toInternalConfig)
import Hatchinq.Button as Button
import Hatchinq.IconButton as IconButton
import Hatchinq.List exposing (roundImage)
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
type Thumbnail
    = Icon String
    | Image String


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
update lift message state =
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
                    Element.el
                        [ Element.width (px 40)
                        , Element.height (px 40)
                        , Element.paddingXY 26 0
                        ]
                        (Element.el [ Element.centerX, Element.centerY ] (icon url))

        headers =
            Element.column
                [ Element.paddingEach { top = 2, bottom = 2, left = 0, right = 8 }
                , Font.family [ theme.font.main ]
                , Font.size theme.font.defaultSize
                , Font.bold
                , Element.width fill
                , Element.height shrink
                , Element.centerY
                , Element.htmlAttribute <| Html.Attributes.style "display" "inline-block"
                , Element.htmlAttribute <| Html.Attributes.style "overflow" "hidden"
                , Element.htmlAttribute <| Html.Attributes.style "text-overflow" "ellipsis"
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
                        Element.el
                            [ Font.size theme.font.smallerSize
                            , Font.color theme.colors.gray.dark
                            , Font.semiBold
                            ]
                            (textWithEllipsis (takeFirstNLines subHead 2))
                ]

        mediaRow =
            Element.row
                [ Element.htmlAttribute <| Html.Attributes.style "overflow" "hidden"
                , Element.width fill
                , Element.height (fill |> Element.maximum 194)
                , Element.centerX
                , Element.centerY
                ]
                [ media ]

        contentRow =
            Element.row
                ([ Element.width fill
                 , Element.htmlAttribute <| Html.Attributes.style "overflow" "auto"
                 , Element.htmlAttribute <| Html.Attributes.style "transition" "transform 0.25s, opacity 0.25s, max-height 0.25s"
                 ]
                    ++ (if (internalConfig.expandable && state.contentExpanded) || not internalConfig.expandable then
                            [ Element.htmlAttribute <| Html.Attributes.style "opacity" "1"
                            , Element.height (shrink |> Element.maximum 194)
                            , Element.paddingEach { left = 0, right = 0, top = 8, bottom = 0 }
                            ]

                        else
                            [ Element.htmlAttribute <| Html.Attributes.style "opacity" "0"
                            , Element.height (shrink |> Element.maximum 0)
                            ]
                       )
                )
                [ content ]

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
                Element.row
                    [ Element.width fill
                    , Element.height (shrink |> Element.minimum 48)
                    , Element.spacing 8
                    , Element.paddingEach { left = 8, right = 8, top = 8, bottom = 8 }
                    , Element.htmlAttribute <| Html.Attributes.style "transition" "box-shadow 0.1s"
                    , Element.htmlAttribute <| Html.Attributes.style "overflow-x" "auto"
                    ]
                    actionsButtons
    in
    Element.column
        ([ Element.centerX
         , Element.centerY
         , Font.family [ theme.font.main ]
         , Font.size theme.font.defaultSize
         , Element.width (fill |> Element.maximum defaultWidth)
         , Element.height shrink
         , Element.htmlAttribute <| Html.Attributes.style "overflow" "hidden"
         , Element.Border.color theme.colors.gray.lightest
         , Element.Border.shadow { offset = ( 0, 0 ), size = 0, blur = 3, color = Element.rgba255 140 140 140 0.58 }
         , Element.mouseOver [ Background.color theme.colors.gray.lightest, Element.Border.shadow { offset = ( 0, 2 ), size = 2, blur = 4, color = Element.rgba255 140 140 140 0.58 } ]
         , Element.Border.rounded 4
         , Element.htmlAttribute <| Html.Attributes.style "transition" "box-shadow 0.1s"
         ]
            ++ toElement attributes
        )
        (case internalConfig.layout of
            MediaCenter ->
                [ Element.row
                    [ Element.width (fill |> Element.minimum 344)
                    , Element.height (shrink |> Element.maximum 80)
                    , Element.paddingEach { left = 0, right = 0, top = 16, bottom = 16 }
                    , Element.centerY
                    ]
                    [ imageOrIcon
                    , headers
                    ]
                , mediaRow
                , Element.row
                    [ Element.width fill
                    , if internalConfig.expandable then
                        if state.contentExpanded then
                            Element.paddingEach { left = 16, right = 16, top = 8, bottom = 0 }

                        else
                            Element.paddingEach { left = 16, right = 16, top = 0, bottom = 0 }

                      else
                        Element.paddingEach { left = 16, right = 16, top = 8, bottom = 0 }
                    ]
                    [ contentRow ]
                , Element.row [ Element.width fill ]
                    [ buttonsRow
                    , if internalConfig.expandable then
                        Element.el [ Element.paddingXY 8 8, Element.alignRight ] toggleContentButton

                      else
                        Element.none
                    ]
                ]

            MediaTop ->
                [ mediaRow
                , Element.row
                    [ Element.width (fill |> Element.minimum 344)
                    , Element.height (shrink |> Element.maximum 80)
                    , if internalConfig.expandable then
                        if state.contentExpanded then
                            Element.paddingEach { left = 0, right = 0, top = 8, bottom = 8 }

                        else
                            Element.paddingEach { left = 0, right = 0, top = 8, bottom = 0 }

                      else
                        Element.paddingEach { left = 0, right = 0, top = 8, bottom = 8 }
                    , Element.centerY
                    ]
                    [ imageOrIcon
                    , headers
                    , if internalConfig.expandable then
                        Element.el [ Element.paddingXY 8 0 ] toggleContentButton

                      else
                        Element.none
                    ]
                , Element.row
                    [ Element.width fill
                    , if buttonsRow == Element.none then
                        Element.paddingEach { left = 16, right = 16, top = 0, bottom = 8 }

                      else
                        Element.paddingEach { left = 16, right = 16, top = 0, bottom = 0 }
                    ]
                    [ contentRow ]
                , buttonsRow
                ]
        )
