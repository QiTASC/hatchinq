module Hatchinq.Card exposing (Config, Layout(..), Message(..), State, Thumbnail(..), View, configure, expandable, init, layout, update)

import Element exposing (Element, centerY, fill, px, shrink)
import Element.Background as Background
import Element.Border
import Element.Font as Font
import Hatchinq.Attribute as Attribute exposing (Attribute, custom, toElement, toInternalConfig)
import Hatchinq.Button as Button
import Hatchinq.IconButton as IconButton
import Hatchinq.List exposing (roundImage)
import Hatchinq.Theme as Theme exposing (Theme, icon, textWithEllipsis)
import Hatchinq.Util exposing (takeFirstTwoLines)
import Html.Attributes


defaultTheme =
    Theme.default


button =
    Button.configure { theme = defaultTheme }


iconButton =
    IconButton.configure { theme = defaultTheme }


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
init : State
init =
    { contentExpanded = False
    }


type Layout
    = MediaTop
    | MediaCenter


type alias Title =
    { head : String
    , subHead : Maybe String
    }


{-| -}
expandable : Attribute InternalConfig
expandable =
    custom (\v -> { v | expandable = True })


{-| -}
layout : Layout -> Attribute InternalConfig
layout msg =
    custom (\v -> { v | layout = msg })


type Message msg
    = ExpandContent
    | HideContent


type Thumbnail
    = Icon String
    | Image String



-- Media = Element msg
-- Title/Subtitle = { title: String, subtitle: Maybe String }
-- TitleIcon = Icon String | Image String
-- Content = Element msg
-- Actions = List (String, msg)


type alias View msg =
    { media : Element msg
    , titles : Title
    , thumbnail : Thumbnail
    , content : Element msg
    , actions : List ( String, msg )
    , state : State
    }


configure : Config msg -> (List (Attribute InternalConfig) -> View msg -> Element msg)
configure config =
    view config



-- UPDATE


{-| -}
update : (Message msg -> msg) -> Message msg -> State -> ( State, Cmd msg )
update lift message state =
    case message of
        ExpandContent ->
            ( { state | contentExpanded = True }, Cmd.none )

        HideContent ->
            ( { state | contentExpanded = False }, Cmd.none )


view : Config msg -> List (Attribute InternalConfig) -> View msg -> Element msg
view config attributes cardView =
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

        externalAttributes =
            toElement attributes

        columnAttributes =
            [ Element.centerX
            , Element.centerY
            , Font.family [ config.theme.font.main ]
            , Font.size config.theme.font.defaultSize
            , Element.width (fill |> Element.maximum defaultWidth)
            , Element.height shrink
            , Element.htmlAttribute <| Html.Attributes.style "overflow" "hidden"
            , Element.Border.color config.theme.colors.gray.lightest
            , Element.Border.shadow { offset = ( 0, 1 ), size = 1, blur = 3, color = Element.rgba255 140 140 140 0.74 }
            , Element.mouseOver [ Background.color config.theme.colors.gray.lightest ]
            , Element.Border.rounded 4
            ]
                ++ externalAttributes

        wordWrapAttributes =
            [ Element.htmlAttribute <| Html.Attributes.style "display" "inline-block"
            , Element.htmlAttribute <| Html.Attributes.style "overflow" "hidden"
            , Element.htmlAttribute <| Html.Attributes.style "white-space" "pre-wrap"
            , Element.htmlAttribute <| Html.Attributes.style "white-space" "-moz-pre-wrap"
            , Element.htmlAttribute <| Html.Attributes.style "white-space" "-pre-wrap"
            , Element.htmlAttribute <| Html.Attributes.style "white-space" "-o-pre-wrap"
            , Element.htmlAttribute <| Html.Attributes.style "white-space" "break-word"
            , Element.htmlAttribute <| Html.Attributes.style "line-height" "0.5"
            ]
    in
    case internalConfig.layout of
        MediaCenter ->
            Element.column
                columnAttributes
                (mediaCenter config cardView wordWrapAttributes internalConfig.expandable)

        MediaTop ->
            Element.column
                columnAttributes
                (mediaTop config cardView wordWrapAttributes internalConfig.expandable)


mediaCenter : Config msg -> View msg -> List (Element.Attribute msg) -> Bool -> List (Element msg)
mediaCenter { theme, lift } { media, titles, thumbnail, content, actions, state } wordWrapAttributes isExpandable =
    let
        buttons =
            List.map (\( text, action ) -> button [ Button.text ] { label = text, onPress = Just action }) actions
    in
    [ Element.row [ Element.height fill, Element.width fill, Element.height (shrink |> Element.maximum 80) ]
        [ case thumbnail of
            Image url ->
                roundImage url

            Icon url ->
                Element.el
                    [ Element.width (px 40)
                    , Element.height (px 40)
                    , centerY
                    , Element.paddingXY 24 0
                    ]
                    (Element.el [ Element.centerX, Element.centerY ] (icon url))
        , case titles.subHead of
            Nothing ->
                Element.column
                    ([ Element.width fill
                     , Element.height fill
                     ]
                        ++ wordWrapAttributes
                    )
                    [ Element.paragraph
                        [ Font.family [ theme.font.main ]
                        , Font.size theme.font.defaultSize
                        , Font.bold
                        , Element.width fill
                        , Element.height shrink
                        , Element.paddingXY 0 16
                        ]
                        [ textWithEllipsis (takeFirstTwoLines titles.head) ]
                    , Element.none
                    ]

            Just subHead ->
                Element.column
                    ([ Element.width fill
                     , Element.height fill
                     ]
                        ++ wordWrapAttributes
                    )
                    [ Element.column [ Element.paddingXY 0 16 ]
                        [ Element.el
                            [ Font.family [ theme.font.main ]
                            , Font.size theme.font.defaultSize
                            , Font.bold
                            , Element.width fill
                            , Element.height shrink
                            ]
                            (textWithEllipsis ((\t -> String.join "\n" (List.take 1 (String.split "\n" t))) titles.head))
                        , Element.paragraph
                            [ Element.width fill
                            , Element.height fill
                            , Font.family [ theme.font.main ]
                            , Font.size theme.font.smallerSize
                            , Font.color theme.colors.gray.dark
                            , Font.semiBold
                            , Element.centerX
                            , Element.centerY
                            ]
                            [ textWithEllipsis (takeFirstTwoLines subHead) ]
                        ]
                    ]
        ]
    , Element.el
        [ Element.htmlAttribute <| Html.Attributes.style "overflow" "hidden"
        , Element.width fill
        , Element.height (shrink |> Element.maximum 194)
        ]
        media
    , case isExpandable of
        True ->
            case state.contentExpanded of
                True ->
                    Element.column [ Element.width fill, Element.height shrink ]
                        [ Element.row
                            [ Font.family [ theme.font.main ]
                            , Font.size theme.font.smallSize
                            , Font.color theme.colors.gray.dark
                            , Element.htmlAttribute <| Html.Attributes.style "overflow" "hidden"
                            , Element.alignBottom
                            , Element.width fill
                            , Element.height shrink
                            , Element.height (shrink |> Element.minimum 60)
                            ]
                            [ Element.el
                                [ Element.height ((shrink |> Element.minimum 60) |> Element.maximum 194)
                                , Element.width fill
                                , Element.paddingXY 16 16
                                , Element.htmlAttribute <| Html.Attributes.style "overflow" "auto"
                                ]
                                content
                            ]
                        , Element.el [ Element.alignRight, Element.paddingXY 8 8 ] (iconButton [] { icon = "expand_less", onPress = Just (lift <| HideContent) })
                        ]

                False ->
                    Element.row [ Element.alignRight, Element.paddingXY 8 8 ] [ Element.el [] (iconButton [] { icon = "expand_more", onPress = Just (lift <| ExpandContent) }) ]

        False ->
            Element.row [ Element.width fill ]
                [ Element.column [ Element.width fill ]
                    [ Element.el
                        [ Font.family [ theme.font.main ]
                        , Font.size theme.font.smallSize
                        , Font.color theme.colors.gray.dark
                        , Element.htmlAttribute <| Html.Attributes.style "overflow" "hidden"
                        , Element.spacing 8
                        , Element.alignBottom
                        , Element.width fill
                        , Element.height (shrink |> Element.maximum 60)
                        ]
                        (Element.el
                            [ Element.paddingXY 16 16
                            , Element.htmlAttribute <| Html.Attributes.style "overflow" "auto"
                            , Element.width fill
                            ]
                            content
                        )
                    , Element.row [ Element.width fill, Element.height fill, Element.spacing 8, Element.paddingXY 8 0, Element.height (shrink |> Element.minimum 48) ] buttons
                    ]
                ]
    ]


mediaTop : Config msg -> View msg -> List (Element.Attribute msg) -> Bool -> List (Element msg)
mediaTop { theme, lift } { media, titles, thumbnail, content, actions, state } wordWrapAttributes isExpandable =
    let
        buttons =
            List.map (\( text, action ) -> button [ Button.text ] { label = text, onPress = Just action }) actions
    in
    [ Element.row [ Element.height shrink, Element.width fill ]
        [ Element.el
            [ Element.htmlAttribute <| Html.Attributes.style "overflow" "hidden"
            , Element.width fill
            , Element.height (fill |> Element.maximum 194)
            , Element.centerX
            , Element.centerY
            , Element.htmlAttribute <| Html.Attributes.style "object-fit" "contained"
            ]
            media
        ]
    , case isExpandable of
        True ->
            case state.contentExpanded of
                True ->
                    Element.column []
                        [ Element.row [ Element.spacing 16, Element.width fill, Element.htmlAttribute <| Html.Attributes.style "overflow" "hidden" ]
                            [ Element.el
                                [ Font.family [ theme.font.main ]
                                , Font.size theme.font.smallSize
                                , Font.color theme.colors.gray.dark
                                , Element.spacing 8
                                , Element.alignBottom
                                , Element.width fill
                                ]
                                (Element.el
                                    [ Element.paddingXY 16 8 ]
                                    content
                                )
                            ]
                        , Element.row
                            [ Element.width fill, Element.height fill, Element.spacing 8, Element.paddingXY 8 0, Element.height (shrink |> Element.maximum 48) ]
                            [ iconButton [] { icon = "expand_less", onPress = Just (lift <| HideContent) } ]
                        ]

                False ->
                    Element.row [ Element.alignRight, Element.paddingXY 8 8 ] [ Element.el [] (iconButton [] { icon = "expand_more", onPress = Just (lift <| ExpandContent) }) ]

        False ->
            Element.column []
                [ Element.row [ Element.spacing 16, Element.width fill, Element.htmlAttribute <| Html.Attributes.style "overflow" "hidden" ]
                    [ Element.el
                        [ Font.family [ theme.font.main ]
                        , Font.size theme.font.smallSize
                        , Font.color theme.colors.gray.dark
                        , Element.spacing 8
                        , Element.alignBottom
                        , Element.width fill
                        ]
                        (Element.el
                            [ Element.paddingXY 16 8 ]
                            content
                        )
                    ]
                , Element.row [ Element.width fill, Element.height fill, Element.spacing 8, Element.paddingXY 8 0, Element.height (shrink |> Element.minimum 48) ] buttons
                ]
    ]



--case titles.subHead of
--        Nothing ->
--            Element.column [ Element.width fill, Element.spacing 8, Element.paddingXY 8 16 ]
--                [ Element.row []
--                    [ case thumbnail of
--                        Image url ->
--                            roundImage url
--
--                        Icon url ->
--                            Element.el
--                                [ Element.width (px 40)
--                                , Element.height (px 40)
--                                ]
--                                (Element.el [ Element.centerX, Element.centerY ] (icon url))
--                    , Element.paragraph
--                        [ Font.family [ theme.font.main ]
--                        , Font.size theme.font.defaultSize
--                        , Font.bold
--                        ]
--                        [ textWithEllipsis (takeFirstTwoLines titles.head) ]
--                    ]
--                , case isExpandable of
--                    True ->
--                        case state.contentExpanded of
--                            True ->
--
--
--                            False ->
--
--
--                    False ->
--                        Element.column []
--                            [ Element.row [ Element.spacing 16, Element.width fill, Element.htmlAttribute <| Html.Attributes.style "overflow" "hidden" ]
--                                [ Element.el
--                                    [ Font.family [ theme.font.main ]
--                                    , Font.size theme.font.smallSize
--                                    , Font.color theme.colors.gray.dark
--                                    , Element.spacing 8
--                                    , Element.alignBottom
--                                    , Element.width fill
--                                    ]
--                                    (Element.el
--                                        [ Element.paddingXY 16 8 ]
--                                        content
--                                    )
--                                ]
--                            , Element.row [ Element.width fill, Element.height fill, Element.spacing 8, Element.paddingXY 8 0, Element.height (shrink |> Element.minimum 48) ] buttons
--                            ]
--                ]
--
--        Just subHead ->
--            Element.column [ Element.width fill ]
--                [ Element.row [ Element.width fill ]
--                    [ case thumbnail of
--                        Image url ->
--                            roundImage url
--
--                        Icon url ->
--                            Element.el
--                                [ Element.width (px 40)
--                                , Element.height (px 40)
--                                , Element.paddingXY 24 0
--                                ]
--                                (Element.el [ Element.centerX, Element.centerY ] (icon url))
--                    , Element.paragraph
--                        ([ Element.paddingXY 0 16 ]
--                            ++ wordWrapAttributes
--                        )
--                        [ Element.el
--                            [ Font.family [ theme.font.main ]
--                            , Font.size theme.font.defaultSize
--                            , Font.bold
--                            ]
--                            (textWithEllipsis ((\t -> String.join "\n" (List.take 1 (String.split "\n" t))) titles.head))
--                        , Element.el
--                            [ Font.family [ theme.font.main ]
--                            , Font.size theme.font.smallerSize
--                            , Font.color theme.colors.gray.dark
--                            , Font.semiBold
--                            ]
--                            (textWithEllipsis (takeFirstTwoLines subHead))
--                        ]
--                    , case isExpandable of
--                        True ->
--                            case state.contentExpanded of
--                                True ->
--                                    Element.column []
--                                        [ Element.row [ Element.spacing 16, Element.width fill, Element.htmlAttribute <| Html.Attributes.style "overflow" "hidden" ]
--                                            [ Element.el
--                                                [ Font.family [ theme.font.main ]
--                                                , Font.size theme.font.smallSize
--                                                , Font.color theme.colors.gray.dark
--                                                , Element.spacing 8
--                                                , Element.alignBottom
--                                                , Element.width fill
--                                                ]
--                                                (Element.el
--                                                    [ Element.paddingXY 16 8 ]
--                                                    content
--                                                )
--                                            ]
--                                        , Element.row
--                                            [ Element.width fill, Element.height fill, Element.spacing 8, Element.paddingXY 8 0, Element.height (shrink |> Element.maximum 48) ]
--                                            [ iconButton [] { icon = "expand_less", onPress = Just (lift <| HideContent) } ]
--                                        ]
--
--                                False ->
--                                    Element.row [ Element.alignRight, Element.paddingXY 8 8 ] [ Element.el [] (iconButton [] { icon = "expand_more", onPress = Just (lift <| ExpandContent) }) ]
--
--                        False ->
--
--                    ]
--                ]
