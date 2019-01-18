module Hatchinq.List exposing (Config, Message, State, View, borderWidthEach, configure, imageSrc, init, secondaryText, update)

{-|


# Exposed

@docs Config, Message, State, View, borderWidthEach, configure, imageSrc, init, secondaryText, update

-}

import Element exposing (Element, centerY, fill, height, html, htmlAttribute, inFront, mouseOver, paddingEach, paddingXY, px, scrollbarY, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Hatchinq.Attribute exposing (Attribute, custom, toElement, toInternalView)
import Hatchinq.Theme exposing (Theme, textWithEllipsis)
import Html
import Html.Attributes
import Task



-- TYPES


{-| -}
type alias State item =
    { id : Maybe String
    , selected : Maybe item
    }


{-| -}
init : State item
init =
    State Nothing Nothing



-- MESSAGES


{-| -}
type Message item msg
    = Select (Maybe item) (Maybe msg)



-- UPDATE


{-| -}
update : Message item msg -> State item -> ( State item, Cmd msg )
update message state =
    case message of
        Select item onSelect ->
            ( { state | selected = item }
            , case onSelect of
                Just msg ->
                    Task.succeed msg |> Task.perform identity

                Nothing ->
                    Cmd.none
            )



-- CONFIG


{-| -}
type alias Config item msg =
    { theme : Theme
    , lift : Message item msg -> msg
    }


{-| -}
configure : Config item msg -> (List (Attribute (InternalConfig item msg)) -> View item msg -> Element msg)
configure config =
    view config


type alias InternalConfig item msg =
    { toSecondaryText : Maybe (item -> String)
    , toImageSrc : Maybe (item -> String)
    , extraExternalAttributes : List (Element.Attribute msg)
    }


{-| -}
secondaryText : (item -> String) -> Attribute (InternalConfig item msg)
secondaryText toSecondaryText =
    custom (\v -> { v | toSecondaryText = Just toSecondaryText })


{-| -}
imageSrc : (item -> String) -> Attribute (InternalConfig item msg)
imageSrc toImageSrc =
    custom (\v -> { v | toImageSrc = Just toImageSrc })


{-| -}
borderWidthEach :
    { bottom : Int
    , left : Int
    , right : Int
    , top : Int
    }
    -> Attribute (InternalConfig item msg)
borderWidthEach borders =
    custom (\v -> { v | extraExternalAttributes = Border.widthEach borders :: v.extraExternalAttributes })



-- VIEW


{-| -}
type alias View item msg =
    { items : List item
    , toPrimaryText : item -> String
    , onSelect : Maybe (item -> msg)
    , activated : Maybe item
    , state : State item
    }


view : Config item msg -> List (Attribute (InternalConfig item msg)) -> View item msg -> Element msg
view config attributes data =
    let
        { theme, lift } =
            config

        defaultInternalConfig =
            { toSecondaryText = Nothing
            , toImageSrc = Nothing
            , extraExternalAttributes = []
            }

        internalConfig =
            defaultInternalConfig |> toInternalView attributes

        externalAttributes =
            toElement attributes

        bodyAttributes =
            [ paddingXY 0 8
            , Border.width 1
            , Border.color theme.colors.gray.light
            , width (px 280)
            ]

        extraAttributes =
            case data.onSelect of
                Nothing ->
                    [ htmlAttribute (Html.Attributes.tabindex -1)
                    , Events.onLoseFocus (lift <| Select Nothing Nothing)
                    ]

                _ ->
                    []
    in
    Element.el ([ scrollbarY ] ++ bodyAttributes ++ extraAttributes ++ externalAttributes ++ internalConfig.extraExternalAttributes)
        (Element.column [ width fill, height fill ] (List.map (\item -> listItem config internalConfig data item) data.items))


listItem : Config item msg -> InternalConfig item msg -> View item msg -> item -> Element msg
listItem { theme, lift } internalConfig data item =
    let
        ( leftPadding, additionalItemAttributes ) =
            case internalConfig.toImageSrc of
                Just toImageSrc ->
                    ( 72
                    , [ case internalConfig.toSecondaryText of
                            Just _ ->
                                height (px 72)

                            Nothing ->
                                height (px 56)
                      , inFront (roundImage (toImageSrc item))
                      ]
                    )

                Nothing ->
                    ( 16
                    , case internalConfig.toSecondaryText of
                        Just _ ->
                            [ height (px 64) ]

                        Nothing ->
                            [ height (px 48) ]
                    )

        colorAttributes =
            if data.activated == Just item then
                Font.color theme.colors.primary.dark
                    :: (htmlAttribute <| Html.Attributes.class "ripple focusPrimaryRipple")
                    :: (if data.state.selected == Just item then
                            [ Background.color theme.colors.primary.lighter ]

                        else
                            [ Background.color theme.colors.primary.lightest
                            , mouseOver [ Background.color theme.colors.primary.lighter ]
                            ]
                       )

            else
                (htmlAttribute <| Html.Attributes.class "ripple focusGrayRipple")
                    :: (if data.state.selected == Just item then
                            [ Background.color theme.colors.gray.light ]

                        else
                            [ mouseOver [ Background.color theme.colors.gray.lightest ] ]
                       )

        itemAttributes =
            [ width fill
            , Font.family [ theme.font.main ]
            , Font.size theme.font.defaultSize
            , Events.onMouseUp (lift <| Select (Just item) (Maybe.map (\onSelect -> onSelect item) data.onSelect))
            ]
                ++ colorAttributes
                ++ additionalItemAttributes

        textAttributes =
            [ width fill
            , paddingEach { top = 0, right = 16, bottom = 0, left = leftPadding }
            , centerY
            ]

        secondaryTextElements =
            case internalConfig.toSecondaryText of
                Just toSecondaryText ->
                    [ Element.el
                        (Font.color theme.colors.gray.color
                            :: Font.size theme.font.smallSize
                            :: textAttributes
                        )
                        (toSecondaryText item |> textWithEllipsis)
                    ]

                Nothing ->
                    []
    in
    Element.column
        itemAttributes
        (Element.el textAttributes (data.toPrimaryText item |> textWithEllipsis)
            :: secondaryTextElements
        )


roundImage : String -> Element msg
roundImage src =
    Element.el [ centerY, paddingXY 16 0 ]
        (html
            (Html.img
                [ Html.Attributes.src src
                , Html.Attributes.alt ""
                , Html.Attributes.width 40
                , Html.Attributes.height 40
                , Html.Attributes.style "border-radius" "50%"
                , Html.Attributes.style "object-fit" "cover"
                ]
                []
            )
        )
