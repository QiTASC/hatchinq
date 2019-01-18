module Hatchinq.MouseDecoder exposing (MousePosition, positionDecoder)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)


type alias MousePosition =
    { x : Int
    , y : Int
    }


positionDecoder : Decode.Decoder MousePosition
positionDecoder =
    Decode.succeed MousePosition
        |> required "pageX" Decode.int
        |> required "pageY" Decode.int
