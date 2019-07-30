module Hatchinq.Divider exposing (Config, InternalConfig, configure, view, withColor)

import Element exposing (Color, Element, el, fill, height, none, padding, px, width)
import Element.Background as Background
import Hatchinq.Attribute exposing (Attribute, custom, toElement, toInternalConfig)
import Hatchinq.Theme exposing (Theme)



-- TYPES


{-| -}
type alias Config =
    { theme : Theme
    }


type alias InternalConfig =
    { color : Color
    }


{-| -}
configure : Config -> (List (Attribute InternalConfig) -> Element msg)
configure config =
    view config


{-| -}
withColor : Color -> Attribute InternalConfig
withColor color =
    custom (\v -> { v | color = color })


{-| -}
view : Config -> List (Attribute InternalConfig) -> Element msg
view { theme } source =
    let
        defaultConfig =
            { color = theme.colors.gray.color
            }

        internalConfig =
            toInternalConfig source defaultConfig

        attributes =
            toElement source
    in
    el [ padding 8, width fill ] <| el ([ height <| px 2, width fill, Background.color internalConfig.color ] ++ attributes) none
