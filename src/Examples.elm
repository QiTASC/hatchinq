module Examples exposing (main)

{-| Exposed

@docs main

-}

import Browser
import Browser.Dom
import Browser.Events
import Element exposing (Element, alignTop, fill, inFront, padding, px, shrink, spacing)
import Element.Border
import Hatchinq.AppBar as AppBar
import Hatchinq.Attribute exposing (Attribute, height, width, withAttributes)
import Hatchinq.Button as Button exposing (..)
import Hatchinq.Checkbox as Checkbox exposing (..)
import Hatchinq.DataTable as DataTable exposing (..)
import Hatchinq.DropDown as DropDown exposing (..)
import Hatchinq.IconButton as IconButton
import Hatchinq.List as MaterialList exposing (..)
import Hatchinq.SidePanel as SidePanel exposing (..)
import Hatchinq.TextField as TextField exposing (..)
import Hatchinq.Theme as Theme exposing (..)
import Html exposing (Html)
import List
import Set exposing (Set)
import Task


{-| -}
main : Program {} Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ SidePanel.subscriptions leftPanelConfig model.leftSidePanelState
        , SidePanel.subscriptions rightPanelConfig model.rightSidePanelState
        , Browser.Events.onResize (\width height -> WindowSizeChanged width height)
        ]


type InputField
    = FirstInputField
    | SecondInputField


type DropDownComponent
    = Default
    | Disabled
    | FullWidth


type ListComponent
    = Simple
    | WithSecondaryText
    | WithImagesAndSelectable
    | WithImagesAndSecondaryTextAndSelectable


type alias Person =
    { id : Int
    , firstName : String
    , lastName : String
    , age : Int
    , additionalInfo : Maybe String
    , imageSrc : String
    }


persons =
    [ Person 0 "Bob" "Sponge" 20 (Just "Awesome guy") "https://vignette.wikia.nocookie.net/lostmedia/images/6/60/Spongebill.png"
    , Person 1 "Morty" "Smith" 29 (Just "Cool guy") "https://qph.fs.quoracdn.net/main-qimg-9a1a3120354b2b345c5e5c6a1647fb6a"
    , Person 2 "Rick" "Sanchez" 40 (Just "Loves Elm") "https://pbs.twimg.com/profile_images/686425525032632320/D6_xAbDK_400x400.jpg"
    , Person 3 "Thanos" "Mad Titan" 35 Nothing "https://www.sideshowtoy.com/wp-content/uploads/2018/04/marvel-avengers-infinity-war-thanos-sixth-scale-figure-hot-toys-feature-903429-1.jpg"
    ]


type Msg
    = PressMinus
    | PressPlus
    | InputChange InputField String
    | InputStateChange (TextField.Message InputField)
    | DataTableChange (DataTable.Message Person Msg)
    | DataTableSortChange Int (Maybe Bool)
    | DataTableSelectionChange (Maybe Person) Bool
    | DataTableExpansionChange Person Bool
    | DropdownValueChange DropDownComponent String
    | DropdownStateChange DropDownComponent (DropDown.Message String Msg)
    | ListStateChange ListComponent (MaterialList.Message Person Msg)
    | ListValueChange ListComponent Person
    | SearchPage
    | OpenUserOptions
    | ToggleNavigation
    | LeftPanelMessage SidePanel.State
    | RightPanelMessage SidePanel.State
    | CheckboxValueChange Bool
    | WindowSizeChanged Int Int
    | Noop


theme =
    Theme.default


appBar =
    AppBar.configure { theme = theme }


checkbox =
    Checkbox.configure { theme = theme }


button =
    Button.configure { theme = theme }


iconButton =
    IconButton.configure { theme = theme }


textButton =
    button |> withAttributes [ Button.text ]


containedButton =
    button |> withAttributes [ Button.contained ]


textField =
    TextField.configure
        { theme = theme
        , lift = InputStateChange
        }


dropDown id attr v =
    DropDown.configure
        { theme = theme
        , lift = DropdownStateChange id
        }
        attr
        v


dataTable =
    DataTable.configure
        { theme = theme
        , lift = DataTableChange
        }


list id attr v =
    MaterialList.configure
        { theme = theme
        , lift = ListStateChange id
        }
        attr
        v


leftPanelConfig =
    { theme = theme
    , lift = LeftPanelMessage
    , orientation = LeftHand
    , maxWidthFraction = 0.33
    }


leftSidePanel =
    SidePanel.configure leftPanelConfig []


rightPanelConfig =
    { theme = theme
    , lift = RightPanelMessage
    , orientation = RightHand
    , maxWidthFraction = 0.33
    }


rightSidePanel =
    SidePanel.configure rightPanelConfig []


type alias Model =
    { counter : Int
    , inputValue : String
    , secondInputValue : String
    , inputField : TextField.State InputField
    , defaultDropdownValue : Maybe String
    , defaultDropdown : DropDown.State
    , disabledDropdownValue : Maybe String
    , disabledDropdown : DropDown.State
    , fullWidthDropdownValue : Maybe String
    , fullWidthDropdown : DropDown.State
    , leftSidePanelState : SidePanel.State
    , rightSidePanelState : SidePanel.State
    , dataTable : DataTable.State Person Msg
    , persons : List Person
    , list1State : MaterialList.State Person
    , list2State : MaterialList.State Person
    , list3State : MaterialList.State Person
    , list4State : MaterialList.State Person
    , list3Value : Maybe Person
    , list4Value : Maybe Person
    , selectedPersons : Set Int
    , expandedPersons : Set Int
    , checkboxValue : Maybe Bool
    , windowSize : ( Int, Int )
    }


init : {} -> ( Model, Cmd Msg )
init _ =
    let
        ( leftPanelState, leftPanelCmd ) =
            SidePanel.init LeftPanelMessage

        ( rightPanelState, rightPanelCmd ) =
            SidePanel.init RightPanelMessage
    in
    ( { counter = 0
      , inputValue = ""
      , secondInputValue = "Some text"
      , inputField = TextField.init
      , defaultDropdownValue = Nothing
      , defaultDropdown = DropDown.init
      , disabledDropdownValue = Nothing
      , disabledDropdown = DropDown.init
      , fullWidthDropdownValue = Nothing
      , fullWidthDropdown = DropDown.init
      , leftSidePanelState = leftPanelState
      , rightSidePanelState = rightPanelState
      , dataTable = DataTable.init
      , persons = persons
      , list1State = MaterialList.init
      , list2State = MaterialList.init
      , list3State = MaterialList.init
      , list4State = MaterialList.init
      , list3Value = Nothing
      , list4Value = Nothing
      , selectedPersons = Set.singleton 1
      , expandedPersons = Set.singleton 2
      , checkboxValue = Nothing
      , windowSize = ( 0, 0 )
      }
    , Cmd.batch
        [ leftPanelCmd
        , rightPanelCmd
        , Browser.Dom.getViewport
            |> Task.perform (\v -> WindowSizeChanged (round v.viewport.width) (round v.viewport.height))
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PressMinus ->
            ( { model | counter = model.counter - 1 }, Cmd.none )

        PressPlus ->
            ( { model | counter = model.counter + 1 }, Cmd.none )

        InputChange fieldId newValue ->
            case fieldId of
                FirstInputField ->
                    ( { model | inputValue = newValue }, Cmd.none )

                SecondInputField ->
                    ( { model | secondInputValue = newValue }, Cmd.none )

        InputStateChange inputMessage ->
            let
                newInputField =
                    TextField.update inputMessage model.inputField
            in
            ( { model | inputField = newInputField }, Cmd.none )

        SearchPage ->
            ( model, Cmd.none )

        OpenUserOptions ->
            ( model, Cmd.none )

        ToggleNavigation ->
            ( model, Cmd.none )

        LeftPanelMessage state ->
            ( { model | leftSidePanelState = state }, Cmd.none )

        RightPanelMessage state ->
            ( { model | rightSidePanelState = state }, Cmd.none )

        DropdownValueChange id newValue ->
            case id of
                Default ->
                    ( { model | defaultDropdownValue = Just newValue }, Cmd.none )

                Disabled ->
                    ( { model | disabledDropdownValue = Just newValue }, Cmd.none )

                FullWidth ->
                    ( { model | fullWidthDropdownValue = Just newValue }, Cmd.none )

        DropdownStateChange id internalMsg ->
            case id of
                Default ->
                    let
                        ( newState, cmd ) =
                            DropDown.update internalMsg model.defaultDropdown
                    in
                    ( { model | defaultDropdown = newState }, cmd )

                Disabled ->
                    let
                        ( newState, cmd ) =
                            DropDown.update internalMsg model.disabledDropdown
                    in
                    ( { model | disabledDropdown = newState }, cmd )

                FullWidth ->
                    let
                        ( newState, cmd ) =
                            DropDown.update internalMsg model.fullWidthDropdown
                    in
                    ( { model | fullWidthDropdown = newState }, cmd )

        DataTableChange internalMsg ->
            let
                ( newState, cmd ) =
                    DataTable.update internalMsg model.dataTable
            in
            ( { model | dataTable = newState }, cmd )

        DataTableSortChange columnIndex sortOrder ->
            let
                newPersons =
                    case sortOrder of
                        Nothing ->
                            persons

                        Just order ->
                            if order then
                                List.sortBy (\p -> p.age) persons

                            else
                                List.reverse (List.sortBy (\p -> p.age) persons)
            in
            ( { model | persons = newPersons }, Cmd.none )

        DataTableSelectionChange maybePerson checked ->
            let
                newSelectedPersons =
                    case maybePerson of
                        Just person ->
                            if checked then
                                Set.insert person.id model.selectedPersons

                            else
                                Set.remove person.id model.selectedPersons

                        Nothing ->
                            if checked then
                                Set.fromList (List.map .id persons)

                            else
                                Set.empty
            in
            ( { model | selectedPersons = newSelectedPersons }, Cmd.none )

        DataTableExpansionChange person expanded ->
            let
                newExpandedPersons =
                    if expanded then
                        Set.insert person.id model.expandedPersons

                    else
                        Set.remove person.id model.expandedPersons
            in
            ( { model | expandedPersons = newExpandedPersons }, Cmd.none )

        ListStateChange id internalMsg ->
            case id of
                Simple ->
                    let
                        ( newState, cmd ) =
                            MaterialList.update internalMsg model.list1State
                    in
                    ( { model | list1State = newState }, cmd )

                WithSecondaryText ->
                    let
                        ( newState, cmd ) =
                            MaterialList.update internalMsg model.list2State
                    in
                    ( { model | list2State = newState }, cmd )

                WithImagesAndSelectable ->
                    let
                        ( newState, cmd ) =
                            MaterialList.update internalMsg model.list3State
                    in
                    ( { model | list3State = newState }, cmd )

                WithImagesAndSecondaryTextAndSelectable ->
                    let
                        ( newState, cmd ) =
                            MaterialList.update internalMsg model.list4State
                    in
                    ( { model | list4State = newState }, cmd )

        ListValueChange id value ->
            case id of
                WithImagesAndSelectable ->
                    ( { model | list3Value = Just value }, Cmd.none )

                WithImagesAndSecondaryTextAndSelectable ->
                    ( { model | list4Value = Just value }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CheckboxValueChange value ->
            ( { model | checkboxValue = Just value }, Cmd.none )

        WindowSizeChanged width height ->
            ( { model | windowSize = ( width, height ) }, Cmd.none )

        Noop ->
            ( model, Cmd.none )



-- todo open navigation


view : Model -> Html Msg
view model =
    Element.layout
        [ inFront
            (appBar [ AppBar.navigate ToggleNavigation, AppBar.elevate True ]
                { title = Element.text "Hatchinq Examples"
                , actions =
                    [ { icon = "search", message = SearchPage }
                    , { icon = "person", message = OpenUserOptions }
                    , { icon = "more_vert", message = OpenUserOptions }
                    ]
                }
            )
        ]
    <|
        Element.column [ Element.width fill, Element.height fill ]
            [ Theme.stylesheet theme
            , AppBar.placeholder []
            , Element.row
                [ Element.height fill
                , Element.width fill
                ]
                [ leftSidePanel
                    { buttons =
                        [ { icon = "apps", title = "Buttons", containerContent = buttonsContent model }
                        , { icon = "folder", title = "Files", containerContent = filesContent model }
                        ]
                    , state = model.leftSidePanelState
                    , topPageOffset = AppBar.appBarHeight
                    }
                , mainContent model
                , rightSidePanel
                    { buttons =
                        [ { icon = "account_balance", title = "Projects", containerContent = filesContent model }
                        , { icon = "settings", title = "Settings", containerContent = filesContent model }
                        ]
                    , state = model.rightSidePanelState
                    , topPageOffset = AppBar.appBarHeight
                    }
                ]
            ]


filesContent : Model -> () -> Element msg
filesContent _ _ =
    Element.text "Hello"


buttonsContent : Model -> () -> Element Msg
buttonsContent model _ =
    Element.column
        [ Element.width fill
        , Element.height fill
        , padding 20
        , spacing 8
        , alignTop
        , Element.scrollbars
        ]
        [ Element.row [ spacing 16 ]
            [ textButton [] { label = "-", onPress = Just PressMinus }
            , Element.text <| String.fromInt model.counter
            , textButton [] { label = "+", onPress = Just PressPlus }
            , textButton [] { label = "DISABLED", onPress = Nothing }
            ]
        , Element.row [ spacing 16 ]
            -- outlined is the default, so we don't need to provide anything here
            [ button [] { label = "-", onPress = Just PressMinus }
            , Element.text <| String.fromInt model.counter
            , button [] { label = "+", onPress = Just PressPlus }
            , button [] { label = "DISABLED", onPress = Nothing }
            ]
        , Element.row [ Element.width fill, spacing 16 ]
            [ containedButton [] { label = "-", onPress = Just PressMinus }
            , Element.text <| String.fromInt model.counter
            , containedButton [] { label = "+", onPress = Just PressPlus }
            , containedButton
                [ width <| fill
                , height <| px 65
                ]
                { label = "DISABLED"
                , onPress = Nothing
                }
            ]
        , Element.row [ spacing 16 ]
            [ iconButton [] { icon = "exposure_neg_1", onPress = Just PressMinus }
            , Element.text <| String.fromInt model.counter
            , iconButton [] { icon = "exposure_plus_1", onPress = Just PressPlus }
            , iconButton [] { icon = "sync_disabled", onPress = Nothing }
            ]
        , Element.row [ spacing 16 ]
            [ iconButton [ IconButton.filled ] { icon = "exposure_neg_1", onPress = Just PressMinus }
            , Element.text <| String.fromInt model.counter
            , iconButton [ IconButton.filled ] { icon = "exposure_plus_1", onPress = Just PressPlus }
            , iconButton [ IconButton.filled ] { icon = "sync_disabled", onPress = Nothing }
            ]
        ]


mainContent : Model -> Element Msg
mainContent model =
    Element.column
        [ Element.width fill
        , Element.height <| px <| Tuple.second model.windowSize - AppBar.appBarHeight
        , padding 20
        , spacing 8
        , alignTop
        , Element.scrollbars
        ]
        [ Element.row [ Element.width fill, spacing 16 ]
            [ textField []
                { id = FirstInputField
                , label = "My input field"
                , value = model.inputValue
                , state = model.inputField
                , onChange = Just (InputChange FirstInputField)
                }
            , textField [ width fill ]
                { id = SecondInputField
                , label = "Second field"
                , value = model.secondInputValue
                , state = model.inputField
                , onChange =
                    if model.checkboxValue == Nothing || model.checkboxValue == Just False then
                        Nothing

                    else
                        Just (InputChange SecondInputField)
                }
            ]
        , Element.row [ Element.width fill, spacing 16 ]
            [ dropDown Default
                [ DropDown.label "Default Dropdown"
                ]
                { items = [ "a very long text that doesn't fit in item", "b", "c" ]
                , itemToString = identity
                , value = model.defaultDropdownValue
                , onChange = Just (\item -> DropdownValueChange Default item)
                , state = model.defaultDropdown
                }
            , checkbox [] { value = model.checkboxValue, onChange = Just (\value -> CheckboxValueChange value) }
            , checkbox []
                { value = Nothing
                , onChange =
                    if model.checkboxValue == Nothing || model.checkboxValue == Just False then
                        Nothing

                    else
                        Just (\_ -> Noop)
                }
            , dropDown Disabled
                [ DropDown.label
                    (if model.checkboxValue == Nothing || model.checkboxValue == Just False then
                        "Disabled Dropdown"

                     else
                        "Enabled Dropdown"
                    )
                , width (px 200)
                ]
                { items = [ "a", "b", "c" ]
                , itemToString = identity
                , value = model.disabledDropdownValue
                , onChange =
                    if model.checkboxValue == Nothing || model.checkboxValue == Just False then
                        Nothing

                    else
                        Just (\item -> DropdownValueChange Disabled item)
                , state = model.disabledDropdown
                }
            , dropDown FullWidth
                [ DropDown.label "Full Width Outlined Dropdown"
                , DropDown.dropDownCount 2
                , width fill
                , DropDown.outlined
                ]
                { items = [ "a", "b", "c", "d" ]
                , itemToString = identity
                , value = model.fullWidthDropdownValue
                , onChange =
                    if model.checkboxValue == Nothing || model.checkboxValue == Just False then
                        Nothing

                    else
                        Just (\item -> DropdownValueChange FullWidth item)
                , state = model.fullWidthDropdown
                }
            ]
        , Element.row [ Element.width fill, spacing 16 ]
            [ Element.el [ Element.height fill ]
                (dataTable
                    [ DataTable.selection (\p -> Set.member p.id model.selectedPersons) (\p checked -> DataTableSelectionChange (Just p) checked) (\allSelected -> DataTableSelectionChange Nothing allSelected)
                    , DataTable.expansion (\p -> Set.member p.id model.expandedPersons) (\p expanded -> DataTableExpansionChange p expanded) (\p -> Element.text (Maybe.withDefault "Nothing" p.additionalInfo))
                    ]
                    { columns =
                        [ DataTable.column (Element.text "First name") (px 100) (\_ person -> Element.text person.firstName)
                        , DataTable.sortableColumn (Element.text "Last name") (px 100) (\_ person -> Element.text person.lastName) (DataTable.Lambda (List.sortBy (\p -> p.lastName)))
                        , DataTable.sortableColumn (Element.text "Age") (px 100) (\_ person -> Element.text (String.fromInt person.age)) (DataTable.Lambda (List.sortBy (\p -> p.age)))
                        ]
                    , items =
                        persons
                    , state = model.dataTable
                    }
                )
            , Element.el [ Element.height fill ]
                (Element.el [ Element.height shrink, Element.width (px 250), Element.scrollbarX ]
                    (dataTable
                        [ DataTable.selection (\p -> Set.member p.id model.selectedPersons) (\p checked -> DataTableSelectionChange (Just p) checked) (\allSelected -> DataTableSelectionChange Nothing allSelected)
                        , width shrink
                        ]
                        { columns =
                            [ DataTable.column (Element.text "First name") (px 100) (\_ person -> Element.text person.firstName)
                            , DataTable.sortableColumn (Element.text "Last name") (px 100) (\_ person -> Element.text person.lastName) (DataTable.Lambda (List.sortBy (\p -> p.lastName)))
                            , DataTable.sortableColumn (Element.text "Age") (px 100) (\_ person -> Element.text (String.fromInt person.age)) (DataTable.Lambda (List.sortBy (\p -> p.age)))
                            ]
                        , items =
                            persons
                        , state = model.dataTable
                        }
                    )
                )
            , Element.el [ Element.height fill, Element.width fill ]
                (dataTable
                    [ height (px 200), width fill ]
                    { columns =
                        [ DataTable.column (Element.text "First name") (fill |> Element.minimum 100) (\_ person -> Element.text person.firstName)
                        , DataTable.column (Element.text "Last name") (fill |> Element.minimum 100) (\_ person -> Element.text person.lastName)
                        , DataTable.sortableColumn (Element.text "Age") (fill |> Element.minimum 100) (\_ person -> Element.text (String.fromInt person.age)) (DataTable.Update DataTableSortChange)
                        ]
                    , items =
                        model.persons
                    , state = model.dataTable
                    }
                )
            ]
        , Element.row [ Element.width fill, spacing 16 ]
            [ Element.el [ Element.height fill ]
                (Element.el [ Element.Border.width 1, Element.Border.color theme.colors.gray.light ]
                    (list WithImagesAndSecondaryTextAndSelectable
                        [ imageSrc (\person -> person.imageSrc)
                        , secondaryText (\person -> Maybe.withDefault "" person.additionalInfo)
                        , itemsCount 2
                        ]
                        { items = persons
                        , toPrimaryText = \person -> person.firstName ++ " " ++ person.lastName
                        , onSelect = Just (\person -> ListValueChange WithImagesAndSecondaryTextAndSelectable person)
                        , activated = model.list4Value
                        , state = model.list4State
                        }
                    )
                )
            , Element.el [ Element.height fill ]
                (Element.el [ Element.Border.width 1, Element.Border.color theme.colors.gray.light ]
                    (list WithImagesAndSelectable
                        [ imageSrc (\person -> person.imageSrc) ]
                        { items = persons
                        , toPrimaryText = \person -> person.firstName ++ " " ++ person.lastName
                        , onSelect = Just (\person -> ListValueChange WithImagesAndSelectable person)
                        , activated = model.list3Value
                        , state = model.list3State
                        }
                    )
                )
            , Element.el [ Element.height fill ]
                (Element.el [ Element.Border.width 1, Element.Border.color theme.colors.gray.light ]
                    (list WithSecondaryText
                        [ secondaryText (\person -> Maybe.withDefault "" person.additionalInfo)
                        , width (px 200)
                        , height (px 150)
                        ]
                        { items = persons
                        , toPrimaryText = \person -> person.firstName ++ " " ++ person.lastName
                        , onSelect = Nothing
                        , activated = Nothing
                        , state = model.list2State
                        }
                    )
                )
            , Element.el [ Element.height fill ]
                (Element.el [ Element.Border.width 1, Element.Border.color theme.colors.gray.light ]
                    (list Simple
                        [ width (px 150)
                        , itemsCount 5
                        ]
                        { items = persons
                        , toPrimaryText = \person -> person.firstName ++ " " ++ person.lastName
                        , onSelect = Nothing
                        , activated = Nothing
                        , state = model.list1State
                        }
                    )
                )
            ]
        ]
