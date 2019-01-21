module Hatchinq.Color exposing (Color, alpha, blue, green, isBrighter, isDarker, red, rgba, toElement, withAlpha, textColor)

{-|


# Exposed

@docs Color, alpha, blue, green, isBrighter, isDarker, red, rgba, toElement, withAlpha, textColor

-}

import Element


{-| -}
type Color
    = CC Int Int Int Float


{-| -}
red : Color -> Int
red (CC r _ _ _) =
    r


{-| -}
green : Color -> Int
green (CC _ g _ _) =
    g


{-| -}
blue : Color -> Int
blue (CC _ _ b _) =
    b


{-| -}
alpha : Color -> Float
alpha (CC _ _ _ a) =
    a


{-| -}
rgb : Int -> Int -> Int -> Color
rgb r g b =
    CC r g b 1


{-| -}
rgba : Int -> Int -> Int -> Float -> Color
rgba =
    CC


{-| -}
withAlpha : Float -> Color -> Color
withAlpha a (CC r g b _) =
    CC r g b a


{-| -}
toElement : Color -> Element.Color
toElement (CC r g b a) =
    Element.rgba255 r g b a


{-| -}
isBrighter : Color -> Color -> Bool
isBrighter (CC lr lg lb la) (CC rr rg rb ra) =
    (toFloat (lr + lg + lb) / la) > (toFloat (rr + rg + rb) / ra)


{-| -}
isDarker : Color -> Color -> Bool
isDarker lhs rhs =
    not <| isBrighter lhs rhs


{-| -}
textColor : Color -> Color
textColor background =
    let
        isBright =
            isBrighter background (rgba 255 255 255 0.5)
    in
    if isBright then
        rgba 0 0 0 1

    else
        rgba 255 255 255 1
