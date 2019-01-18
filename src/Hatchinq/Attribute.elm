module Hatchinq.Attribute exposing
    ( Attribute
    , custom
    , height
    , none
    , toElement
    , toInternalView
    , toWidth
    , width
    , withAttributes
    )

import Element exposing (Element, Length)


type Attribute v
    = None
    | Width Length
    | Height Length
    | Custom (v -> v)


none : Attribute v
none =
    None


custom : (v -> v) -> Attribute v
custom f =
    Custom f


width : Length -> Attribute v
width =
    Width


height : Length -> Attribute v
height =
    Height


toInternalView : List (Attribute v) -> v -> v
toInternalView source default =
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

                    _ ->
                        Nothing
            )


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


withAttributes : List (Attribute internal) -> (List (Attribute internal) -> view -> Element msg) -> (List (Attribute internal) -> view -> Element msg)
withAttributes defaultAttributes orig =
    \attr v ->
        orig (attr ++ defaultAttributes) v
