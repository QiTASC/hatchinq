module Hatchinq.Attribute exposing
    ( Attribute
    , custom, height, id, none, toElement, toInternalConfig, toId, toWidth, width, withAttributes
    , toHeight
    )

{-|


# Exposed

@docs Attribute
@docs custom, height, id, none, toElement, toInternalConfig, toId, toWidth, width, withAttributes, toHeight

-}

import Element exposing (Element, Length)
import Html.Attributes


{-| -}
type Attribute v
    = None
    | Width Length
    | Height Length
    | Id String
    | Custom (v -> v)


{-| -}
none : Attribute v
none =
    None


{-| -}
custom : (v -> v) -> Attribute v
custom f =
    Custom f


{-| -}
width : Length -> Attribute v
width =
    Width


{-| -}
height : Length -> Attribute v
height =
    Height


{-| -}
id : String -> Attribute v
id idString =
    Id idString


{-| -}
toInternalConfig : List (Attribute v) -> v -> v
toInternalConfig source default =
    source
        |> List.foldr
            (\item view ->
                case item of
                    Custom f ->
                        view |> f

                    _ ->
                        view
            )
            default


{-| -}
toElement : List (Attribute v) -> List (Element.Attribute msg)
toElement source =
    source
        |> List.filterMap
            (\i ->
                case i of
                    Width l ->
                        Just <| Element.width l

                    Height l ->
                        Just <| Element.height l

                    Id idString ->
                        Just <| Element.htmlAttribute <| Html.Attributes.id idString

                    _ ->
                        Nothing
            )


{-| -}
toWidth : List (Attribute v) -> Maybe Length
toWidth source =
    List.head
        (List.reverse
            (source
                |> List.filterMap
                    (\i ->
                        case i of
                            Width l ->
                                Just <| l

                            _ ->
                                Nothing
                    )
            )
        )


{-| -}
toHeight : List (Attribute v) -> Maybe Length
toHeight source =
    List.head
        (List.reverse
            (source
                |> List.filterMap
                    (\i ->
                        case i of
                            Height l ->
                                Just <| l

                            _ ->
                                Nothing
                    )
            )
        )


{-| -}
toId : List (Attribute v) -> Maybe String
toId source =
    List.head
        (List.reverse
            (source
                |> List.filterMap
                    (\a ->
                        case a of
                            Id i ->
                                Just <| i

                            _ ->
                                Nothing
                    )
            )
        )


{-| -}
withAttributes : List (Attribute internal) -> (List (Attribute internal) -> view -> Element msg) -> (List (Attribute internal) -> view -> Element msg)
withAttributes defaultAttributes orig =
    \attr v ->
        orig (attr ++ defaultAttributes) v
