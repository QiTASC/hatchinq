module Hatchinq.Tree exposing (Config, Message, State, TreeNode, View, configure, init, node, update)

{-|


# Exposed

@docs Config, Message, State, TreeNode, View, configure, init, node, update

-}

import Dict exposing (Dict)
import Element exposing (Element, Length, centerX, centerY, fill, height, htmlAttribute, pointer, px, text, width)
import Element.Events exposing (onClick)
import Hatchinq.Attribute exposing (Attribute, toElement)
import Hatchinq.IconButton as IconButton
import Hatchinq.Theme exposing (Theme, arrowTransition)
import Html.Attributes exposing (style)



-- TYPES


{-| -}
type alias Config msg =
    { theme : Theme
    , lift : Message -> msg
    }


{-| -}
type alias State =
    { rootExpandedNode : ExpandedNode
    }


type ExpandedNode
    = ExpandedNode
        { expandedNodes : Dict Int ExpandedNode
        }


{-| -}
init : State
init =
    { rootExpandedNode = ExpandedNode { expandedNodes = Dict.empty }
    }


{-| -}
type TreeNode msg
    = TreeNode
        { text : String
        , onClick : msg
        , children : List (TreeNode msg)
        }


type alias TreePath =
    List Int


{-| -}
node : { text : String, onClick : msg, children : List (TreeNode msg) } -> TreeNode msg
node { text, onClick, children } =
    TreeNode { text = text, onClick = onClick, children = children }



-- MESSAGES


{-| -}
type Message
    = Toggle TreePath



-- UPDATE


{-| -}
update : Message -> State -> State
update message state =
    case message of
        Toggle path ->
            { state | rootExpandedNode = toggleTreeNodeAtPath state.rootExpandedNode path }


toggleTreeNodeAtPath : ExpandedNode -> TreePath -> ExpandedNode
toggleTreeNodeAtPath expandedNode path =
    let
        (ExpandedNode { expandedNodes }) =
            expandedNode
    in
    case path of
        [] ->
            expandedNode

        targetNodeIndex :: nextLayerPath ->
            case Dict.get targetNodeIndex expandedNodes of
                Just childNode ->
                    if List.isEmpty nextLayerPath then
                        ExpandedNode { expandedNodes = Dict.remove targetNodeIndex expandedNodes }

                    else
                        ExpandedNode { expandedNodes = Dict.insert targetNodeIndex (toggleTreeNodeAtPath childNode nextLayerPath) expandedNodes }

                Nothing ->
                    let
                        newChildren =
                            toggleTreeNodeAtPath (ExpandedNode { expandedNodes = Dict.empty }) nextLayerPath
                    in
                    ExpandedNode { expandedNodes = Dict.insert targetNodeIndex newChildren expandedNodes }



-- VIEW


{-| -}
type alias View msg =
    { state : State
    , data : List (TreeNode msg)
    }


{-| -}
configure : Config msg -> List (Attribute v) -> View msg -> Element msg
configure config =
    view config


view : Config msg -> List (Attribute v) -> View msg -> Element msg
view config attributes { state, data } =
    let
        elementAttributes =
            toElement attributes

        (ExpandedNode { expandedNodes }) =
            state.rootExpandedNode
    in
    Element.column
        (width fill
            :: elementAttributes
        )
        (List.indexedMap (\index treeNode -> renderTreeNode config [ index ] (Dict.get index expandedNodes) treeNode) data)


renderTreeNode : Config msg -> TreePath -> Maybe ExpandedNode -> TreeNode msg -> Element msg
renderTreeNode config path maybeExpandedNode (TreeNode { text, onClick, children }) =
    let
        itemRowHeight =
            config.theme.sizes.minRowHeight

        toggleButton =
            if List.isEmpty children then
                Element.el
                    [ width <| px itemRowHeight
                    , height <| px itemRowHeight
                    ]
                    Element.none

            else
                Element.el
                    ([ width (px itemRowHeight)
                     , htmlAttribute <| Html.Attributes.style "will-change" "transform"
                     , htmlAttribute arrowTransition
                     , pointer
                     ]
                        ++ (case maybeExpandedNode of
                                Just _ ->
                                    [ htmlAttribute <| Html.Attributes.style "transform" "rotate(90deg)" ]

                                Nothing ->
                                    []
                           )
                    )
                    (Element.el [ centerX, centerY ]
                        (IconButton.configure { theme = config.theme }
                            []
                            { icon = "arrow_right", onPress = Just (config.lift <| Toggle path) }
                        )
                    )

        childrenElements =
            case maybeExpandedNode of
                Nothing ->
                    Element.none

                Just (ExpandedNode { expandedNodes }) ->
                    if List.isEmpty children then
                        Element.none

                    else
                        Element.column
                            [ Element.paddingEach { top = 0, right = 0, bottom = 0, left = 24 }
                            , width fill
                            ]
                            (List.indexedMap
                                (\nodeIndex childNode ->
                                    renderTreeNode config (path ++ [ nodeIndex ]) (Dict.get nodeIndex expandedNodes) childNode
                                )
                                children
                            )
    in
    Element.column
        [ width fill ]
        [ Element.row
            [ height (px itemRowHeight) ]
            [ toggleButton
            , Element.el
                [ Element.paddingEach { top = 0, right = 4, bottom = 0, left = 0 }
                , Element.Events.onClick <| onClick
                , htmlAttribute <| style "cursor" "default"
                ]
                (Element.text <| text)
            ]
        , childrenElements
        ]
