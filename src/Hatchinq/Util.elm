module Hatchinq.Util exposing (arrowDownKeyCode, arrowLeftKeyCode, arrowRightKeyCode, arrowUpKeyCode, enterKeyCode, escapeKeyCode, keyDownAttribute, keysDownAttribute, takeFirstNLines, onClickPropagation)

import Dict exposing (Dict)
import Element
import Html.Events
import Json.Decode as Decode


enterKeyCode =
    13


arrowLeftKeyCode =
    37


arrowUpKeyCode =
    38


arrowRightKeyCode =
    39


arrowDownKeyCode =
    40


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
                    Decode.succeed message

                Nothing ->
                    Decode.fail "no keyCode found"
    in
    Element.htmlAttribute <| Html.Events.on "keydown" (Decode.andThen isKey Html.Events.keyCode)


keyDownAttribute : Int -> msg -> Element.Attribute msg
keyDownAttribute keyCode message =
    let
        isKey code =
            if code == keyCode then
                Decode.succeed message

            else
                Decode.fail ("not keyCode " ++ String.fromInt keyCode)
    in
    Element.htmlAttribute <| Html.Events.on "keydown" (Decode.andThen isKey Html.Events.keyCode)


takeFirstNLines : String -> Int -> String
takeFirstNLines text numLines =
    String.join "\n" (List.take numLines (String.split "\n" text))


onClickPropagation : Bool -> msg -> Element.Attribute msg
onClickPropagation noPropagation message =
    Element.htmlAttribute <| Html.Events.custom "click" (Decode.succeed { message = message, stopPropagation = noPropagation, preventDefault = True })

