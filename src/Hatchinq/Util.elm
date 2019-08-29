module Hatchinq.Util exposing (arrowDownKeyCode, arrowLeftKeyCode, arrowRightKeyCode, arrowUpKeyCode, enterKeyCode, escapeKeyCode, keyDownAttribute, keysDownAttribute, onClickWithoutPropagation, outsideTarget, takeFirstNLines)

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


onClickWithoutPropagation : Bool -> msg -> Element.Attribute msg
onClickWithoutPropagation noPropagation message =
    Element.htmlAttribute <| Html.Events.custom "click" (Decode.succeed { message = message, stopPropagation = noPropagation, preventDefault = True })


isOutsideTarget : String -> Decode.Decoder Bool
isOutsideTarget targetId =
    Decode.oneOf
        [ Decode.field "id" Decode.string
            |> Decode.andThen
                (\id ->
                    if targetId == id then
                        Decode.succeed False

                    else
                        Decode.fail "continue"
                )
        , Decode.lazy (\_ -> isOutsideTarget targetId |> Decode.field "parentNode")
        , Decode.succeed True
        ]


outsideTarget : String -> Decode.Decoder Bool
outsideTarget targetId =
    Decode.field "target" (isOutsideTarget targetId)
        |> Decode.andThen
            (\isOutside ->
                if isOutside then
                    Decode.succeed True

                else
                    Decode.fail ("inside" ++ targetId)
            )
