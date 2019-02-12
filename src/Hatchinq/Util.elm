module Hatchinq.Util exposing (arrowDownKeyCode, arrowUpKeyCode, enterKeyCode, escapeKeyCode, keyDownAttribute, keysDownAttribute)

import Dict exposing (Dict)
import Element
import Html.Events
import Json.Decode


enterKeyCode =
    13


arrowDownKeyCode =
    40


arrowUpKeyCode =
    38


escapeKeyCode =
    27


keysDownAttribute : Dict Int msg -> Element.Attribute msg
keysDownAttribute keyCodes =
    let
        isKey code =
            let
                maybeMessage =
                    Dict.get code keyCodes
            in
            case maybeMessage of
                Just message ->
                    Json.Decode.succeed message

                Nothing ->
                    Json.Decode.fail "no keyCode found"
    in
    Element.htmlAttribute <| Html.Events.on "keydown" (Json.Decode.andThen isKey Html.Events.keyCode)


keyDownAttribute : Int -> msg -> Element.Attribute msg
keyDownAttribute keyCode message =
    let
        isKey code =
            if code == keyCode then
                Json.Decode.succeed message

            else
                Json.Decode.fail ("not keyCode " ++ String.fromInt keyCode)
    in
    Element.htmlAttribute <| Html.Events.on "keydown" (Json.Decode.andThen isKey Html.Events.keyCode)
