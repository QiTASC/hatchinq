module Hatchinq.Chip exposing
    ( Config
    , Message(..), State, View, configure, dismissible, init, update
    )

{-|


# Exposed

@docs Config

-}

import Element exposing (Element, centerX, centerY, column, height, html, none, paddingEach, px, rgba255, row, shrink, spacing, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Hatchinq.Attribute as Attribute exposing (Attribute, custom, toInternalConfig)
import Hatchinq.Common exposing (Thumbnail(..))
import Hatchinq.IconButton as IconButton exposing (withTextColor)
import Hatchinq.Theme exposing (Theme, icon)
import Html
import Html.Attributes


{-| -}
dismissible : Attribute InternalConfig
dismissible =
    custom (\v -> { v | dismissible = True })



-- TYPES


type alias InternalConfig =
    { dismissible : Bool
    }


{-| -}
type alias Config msg =
    { theme : Theme
    , lift : Message msg -> msg
    }


{-| -}
type alias State =
    { isOpen : Bool
    , id : Int
    }



-- MESSAGES


{-| -}
type Message msg
    = Close Int


{-| -}
init : State
init =
    { isOpen = True
    , id = 0
    }


{-| -}
update : (Message msg -> msg) -> Message msg -> State -> ( State, Cmd msg )
update _ msg state =
    case msg of
        Close id ->
            if id == state.id then
                ( { state | isOpen = False }, Cmd.none )

            else
                ( state, Cmd.none )


{-| -}
type alias View =
    { image : Thumbnail
    , text : String
    , state : State
    }


{-| -}
configure : Config msg -> (List (Attribute InternalConfig) -> View -> Element msg)
configure config =
    view config


view : Config msg -> List (Attribute InternalConfig) -> View -> Element msg
view { theme, lift } attributes { image, text, state } =
    let
        defaultInternalConfig =
            { dismissible = False
            }

        internalConfig =
            toInternalConfig attributes defaultInternalConfig

        iconButton =
            IconButton.configure { theme = theme }

        imageOrIcon =
            case image of
                Image url ->
                    html
                        (Html.img
                            [ Html.Attributes.src url
                            , Html.Attributes.alt ""
                            , Html.Attributes.width 24
                            , Html.Attributes.height 24
                            , Html.Attributes.style "vertical-align" "center"
                            , Html.Attributes.style "border-radius" "50%"
                            , Html.Attributes.style "object-fit" "cover"
                            ]
                            []
                        )

                Icon url ->
                    Html.i [ Html.Attributes.class "material-icons", Html.Attributes.style "user-select" "none" ] [ Html.text url ] |> html

        dismissibleButton =
            if internalConfig.dismissible then
                iconButton
                    [ withTextColor (theme.colors.gray.withAlpha 0.46)
                    , Attribute.width (px 18)
                    , Attribute.height (px 18)
                    , IconButton.fontSize 18
                    ]
                    { icon = "close", onPress = Just (lift <| Close state.id) }

            else
                none
    in
    if state.isOpen then
        row
            [ Border.rounded 50
            , Border.color theme.colors.gray.lightest
            , Border.shadow { offset = ( 0, 0 ), size = 0, blur = 1, color = rgba255 140 140 140 0.58 }
            , Background.color theme.colors.gray.light
            , spacing 8
            , height (px 32)
            ]
            [ column
                [ centerX
                , centerY
                , paddingEach { left = 4, top = 0, right = 0, bottom = 0 }
                ]
                [ imageOrIcon ]
            , column
                [ centerY
                , width shrink
                , Font.family [ theme.font.main ]
                , Font.size theme.font.smallerSize
                ]
                [ Element.text text ]
            , column
                [ centerY
                , paddingEach
                    { left = 0
                    , top = 0
                    , right =
                        if not internalConfig.dismissible then
                            12

                        else
                            8
                    , bottom = 0
                    }
                ]
                [ dismissibleButton ]
            ]

    else
        none
