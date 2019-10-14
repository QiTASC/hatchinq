module Examples exposing (main)

{-| Exposed

@docs main

-}

import Browser
import Browser.Dom
import Browser.Events
import Delay exposing (TimeUnit(..))
import Element exposing (Element, alignTop, below, fill, html, inFront, onLeft, padding, px, shrink, spacing)
import Element.Border
import Element.Events
import Hatchinq.AppBar as AppBar
import Hatchinq.Attribute as Html exposing (Attribute, height, id, width, withAttributes)
import Hatchinq.Button as Button exposing (..)
import Hatchinq.Card as Card exposing (Layout(..), Thumbnail(..))
import Hatchinq.Checkbox as Checkbox exposing (..)
import Hatchinq.DataTable as DataTable exposing (..)
import Hatchinq.Divider as Divider exposing (withColor)
import Hatchinq.DropDown as DropDown exposing (..)
import Hatchinq.IconButton as IconButton exposing (..)
import Hatchinq.List as MaterialList exposing (..)
import Hatchinq.Menu as Menu exposing (MenuItem(..))
import Hatchinq.Paginator as Paginator
import Hatchinq.ProgressIndicator as ProgressIndicator exposing (GrowthDirection(..), Progress(..), circular, linear, startDelaySeconds, visibility)
import Hatchinq.RadioButton as RadioButton
import Hatchinq.SidePanel as SidePanel exposing (..)
import Hatchinq.Snackbar as Snackbar exposing (Content(..))
import Hatchinq.TabBar as TabBar exposing (TabButtons(..))
import Hatchinq.TextField as TextField exposing (..)
import Hatchinq.Theme as Theme exposing (..)
import Hatchinq.Tree as Tree
import Html exposing (Html)
import Html.Attributes
import Json.Decode as Json exposing (string)
import List
import Set exposing (Set)
import Task
import Time exposing (Posix, posixToMillis)


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
        , Menu.subscriptions "appbar-menu" model.menuState MenuLift

        --, Time.every 10 Tick
        ]


type TabType
    = MainTab
    | SettingsTab
    | CustomTab


type InputField
    = FirstInputField
    | SecondInputField
    | ThirdInputField
    | FirstMultiline
    | SecondMultiline
    | ErrorInputField


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
    | MenuLift (Menu.Message Msg)
    | InputChange InputField String
    | InputStateChange (TextField.Message InputField)
    | InputKeyDown String
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
    | SelectPerson (Maybe Person)
    | FilesTreeLift Tree.Message
    | LoadPeople LoadingDirection
    | GotPeople LoadingDirection
    | Tick Posix
    | SnackbarLift (Snackbar.Message Msg)
    | SnackbarAlert (Content Msg)
    | TabBarLift (TabBar.Message Msg)
    | TabBarSelect TabType
    | CardLift (Card.Message Msg)
    | NoOp


theme =
    Theme.default


appBar =
    AppBar.configure { theme = theme }


checkbox =
    Checkbox.configure { theme = theme }


radioButton =
    RadioButton.configure { theme = theme }


button =
    Button.configure { theme = theme }


iconButton =
    IconButton.configure { theme = theme }


textButton =
    button |> withAttributes [ Button.text ]


containedButton =
    button |> withAttributes [ Button.contained ]


menu =
    Menu.configure
        { theme = theme
        , lift = MenuLift
        }


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


denseDataTable =
    DataTable.configure
        { theme = dense theme
        , lift = DataTableChange
        }


paginator =
    Paginator.configure
        { theme = theme }


progressIndicator =
    ProgressIndicator.configure { theme = theme }


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


tree =
    Tree.configure { theme = theme, lift = FilesTreeLift }


denseTree =
    Tree.configure { theme = dense theme, lift = FilesTreeLift }


snackbar =
    Snackbar.configure { theme = theme, lift = SnackbarLift }


tabBar =
    TabBar.configure { theme = theme, lift = TabBarLift }


card =
    Card.configure { theme = theme, lift = CardLift }


divider =
    Divider.configure { theme = theme }


type alias Model =
    { counter : Int
    , inputValue : String
    , secondInputValue : String
    , thirdInputValue : String
    , multilineValue : String
    , secondMultilineValue : String
    , errorInputValue : String
    , inputField : TextField.State InputField
    , defaultDropdownValue : Maybe String
    , defaultDropdown : DropDown.State
    , disabledDropdownValue : Maybe String
    , disabledDropdown : DropDown.State
    , fullWidthDropdownValue : Maybe String
    , fullWidthDropdown : DropDown.State
    , leftSidePanelState : SidePanel.State
    , rightSidePanelState : SidePanel.State
    , dataTable : DataTable.State Person
    , persons : List Person
    , infinitePersons : List Person
    , loadingTop : Maybe Int
    , loadingBottom : Maybe Int
    , list1State : MaterialList.State Person
    , list2State : MaterialList.State Person
    , list3State : MaterialList.State Person
    , list4State : MaterialList.State Person
    , list3Value : Maybe Person
    , list4Value : Maybe Person
    , selectedPersons : Set Int
    , expandedPersons : Set Int
    , checkboxValue : Maybe Bool
    , selectedPerson : Maybe Person
    , filesTreeState : Tree.State
    , snackbarState : Snackbar.State Msg
    , selectedTab : TabType
    , tabBarState : TabBar.State
    , windowSize : ( Int, Int )
    , cardState : Card.State
    , progressIndicator1 : Progress
    , progressIndicatorVisiblity1 : Bool
    , progressIndicator2 : Progress
    , progressIndicatorVisiblity2 : Bool
    , menuState : Menu.State
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
      , multilineValue = ""
      , thirdInputValue = ""
      , secondMultilineValue = "Some text\nOn multiple lines"
      , errorInputValue = "abcd"
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
      , infinitePersons = persons ++ persons ++ persons
      , loadingTop = Nothing
      , loadingBottom = Nothing
      , list1State = MaterialList.init
      , list2State = MaterialList.init
      , list3State = MaterialList.init
      , list4State = MaterialList.init
      , list3Value = Nothing
      , list4Value = Nothing
      , selectedPersons = Set.singleton 1
      , expandedPersons = Set.singleton 2
      , checkboxValue = Nothing
      , selectedPerson = Nothing
      , filesTreeState = Tree.init
      , snackbarState = Snackbar.init
      , cardState = Card.init
      , selectedTab = MainTab
      , tabBarState = TabBar.init
      , windowSize = ( 0, 0 )
      , progressIndicator1 = Determinate 0
      , progressIndicatorVisiblity1 = True
      , progressIndicator2 = Indeterminate
      , progressIndicatorVisiblity2 = True
      , menuState = Menu.init
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

        MenuLift message ->
            let
                ( newMenuState, cmd ) =
                    Menu.update message model.menuState
            in
            ( { model | menuState = newMenuState }, cmd )

        InputChange fieldId newValue ->
            case fieldId of
                FirstInputField ->
                    ( { model | inputValue = newValue }, Cmd.none )

                SecondInputField ->
                    ( { model | secondInputValue = newValue }, Cmd.none )

                ThirdInputField ->
                    ( { model | thirdInputValue = newValue }, Cmd.none )

                FirstMultiline ->
                    ( { model | multilineValue = newValue }, Cmd.none )

                SecondMultiline ->
                    ( { model | secondMultilineValue = newValue }, Cmd.none )

                ErrorInputField ->
                    ( { model | errorInputValue = newValue }, Cmd.none )

        InputStateChange inputMessage ->
            let
                newInputField =
                    TextField.update inputMessage model.inputField
            in
            ( { model | inputField = newInputField }, Cmd.none )

        InputKeyDown _ ->
            ( model, Cmd.none )

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

        DataTableSortChange _ sortOrder ->
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

        SelectPerson person ->
            ( { model | selectedPerson = person }, Cmd.none )

        WindowSizeChanged width height ->
            ( { model | windowSize = ( width, height ) }, Cmd.none )

        FilesTreeLift message ->
            let
                newFilesTreeState =
                    Tree.update message model.filesTreeState
            in
            ( { model | filesTreeState = newFilesTreeState }, Cmd.none )

        LoadPeople direction ->
            let
                totalItemsCount =
                    List.length model.infinitePersons + Maybe.withDefault 0 model.loadingTop + Maybe.withDefault 0 model.loadingBottom

                deltaItemsCount =
                    List.length persons
            in
            case direction of
                Up ->
                    ( { model | loadingTop = Just deltaItemsCount }
                    , Delay.after 1000 Millisecond (GotPeople Up)
                    )

                Down ->
                    ( { model | loadingBottom = Just deltaItemsCount }
                    , Delay.after 1000 Millisecond (GotPeople Down)
                    )

        GotPeople direction ->
            case direction of
                Up ->
                    ( { model | infinitePersons = model.infinitePersons ++ persons, loadingTop = Nothing }, Cmd.none )

                Down ->
                    ( { model | infinitePersons = model.infinitePersons ++ persons, loadingBottom = Nothing }, Cmd.none )

        Tick posix ->
            let
                millis =
                    posixToMillis posix

                progress1 =
                    toFloat (remainderBy 4000 millis) / 20

                progressVisibility =
                    if progress1 > 100 then
                        False

                    else
                        True
            in
            ( { model
                | progressIndicator1 = Determinate progress1
                , progressIndicatorVisiblity1 = progressVisibility
                , progressIndicatorVisiblity2 = progressVisibility
              }
            , Cmd.none
            )

        SnackbarLift internalMsg ->
            let
                ( state, cmd ) =
                    Snackbar.update SnackbarLift internalMsg model.snackbarState
            in
            ( { model | snackbarState = state }, cmd )

        SnackbarAlert content ->
            ( model, Snackbar.alert SnackbarLift content )

        TabBarLift internalMsg ->
            let
                ( state, cmd ) =
                    TabBar.update TabBarLift internalMsg model.tabBarState
            in
            ( { model | tabBarState = state }, cmd )

        TabBarSelect tab ->
            ( { model | selectedTab = tab }, Cmd.none )

        CardLift internalMsg ->
            let
                ( state, cmd ) =
                    Card.update CardLift internalMsg model.cardState
            in
            ( { model | cardState = state }, cmd )

        NoOp ->
            ( model, Cmd.none )



-- todo open navigation


noOutline =
    Element.focusStyle
        { borderColor = Nothing
        , backgroundColor = Nothing
        , shadow = Nothing
        }


view : Model -> Html Msg
view model =
    Element.layoutWith { options = [ noOutline ] }
        [ inFront
            (appBar [ AppBar.navigate ToggleNavigation, AppBar.elevate True ]
                { title = Element.text "Hatchinq Examples"
                , buttons =
                    [ { id = Just "appbar-button-1", icon = "search", message = SearchPage, attributes = [] }
                    , { id = Just "appbar-button-2", icon = "person", message = NoOp, attributes = [] }
                    , { id = Just "appbar-menu-button"
                      , icon = "more_vert"
                      , message = menuToggle model
                      , attributes = menuContent "appbar-menu" model
                      }
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
                        [ { id = Just "buttons-tab-button", icon = "apps", title = "Buttons", containerContent = buttonsContent model }
                        , { id = Just "files-tab-button", icon = "folder", title = "Files", containerContent = filesContent model }
                        ]
                    , state = model.leftSidePanelState
                    , topPageOffset = AppBar.appBarHeight
                    }
                , mainContent model
                , rightSidePanel
                    { buttons =
                        [ { id = Just "projects-tab-button", icon = "account_balance", title = "Projects", containerContent = filesContent model }
                        , { id = Just "settings-tab-button", icon = "settings", title = "Settings", containerContent = filesContent model }
                        ]
                    , state = model.rightSidePanelState
                    , topPageOffset = AppBar.appBarHeight
                    }
                ]
            ]


menuToggle : Model -> Msg
menuToggle model =
    MenuLift
        (if model.menuState.isOpen then
            Menu.CloseMenu Nothing

         else
            Menu.OpenMenu
        )


menuContent : String -> Model -> List (Element.Attribute Msg)
menuContent menuId model =
    [ below
        (Element.el
            [ Element.moveRight 36
            , onLeft
                (menu []
                    { id = menuId
                    , state = model.menuState
                    , items =
                        [ TextItem "Projects" (SnackbarAlert (Plain "You clicked on Projects"))
                        , DividerItem
                        , IconItem "people" "Users" (SnackbarAlert (Plain "You clicked on Users"))
                        , IconItem "settings" "Settings" (SnackbarAlert (Plain "You clicked on Settings"))
                        ]
                    }
                )
            ]
            Element.none
        )
    ]


filesContent : Model -> () -> Element Msg
filesContent model _ =
    let
        data =
            [ Tree.node
                { text = "Documents"
                , onClick = NoOp
                , children = []
                }
            , Tree.node
                { text = "Videos"
                , onClick = NoOp
                , children =
                    [ Tree.node
                        { text = "qitasc.mp4"
                        , onClick = NoOp
                        , children = []
                        }
                    , Tree.node
                        { text = "Tutorials"
                        , onClick = NoOp
                        , children =
                            [ Tree.node
                                { text = "intro.mp4"
                                , onClick = NoOp
                                , children = []
                                }
                            ]
                        }
                    ]
                }
            , Tree.node
                { text = "Projects"
                , onClick = NoOp
                , children = []
                }
            ]
    in
    Element.column
        [ Element.width fill
        , Element.height fill
        , Element.paddingXY 0 8
        , Element.spacing 8
        ]
        [ Element.el [ Element.width fill, Element.Border.width 1, Element.Border.color theme.colors.gray.light ]
            (tree []
                { state = model.filesTreeState
                , data = data
                }
            )
        , Element.el [ Element.width fill, Element.Border.width 1, Element.Border.color theme.colors.gray.light ]
            (denseTree []
                { state = model.filesTreeState
                , data = data
                }
            )
        ]


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
            [ iconButton [ width (px 24), height (px 24), IconButton.filled ] { icon = "exposure_neg_1", onPress = Just PressMinus }
            , Element.text <| String.fromInt model.counter
            , iconButton [ IconButton.filled ] { icon = "exposure_plus_1", onPress = Just PressPlus }
            , iconButton [ width (px 70), height (px 70), IconButton.filled ] { icon = "sync_disabled", onPress = Nothing }
            ]
        , Element.row [ spacing 16 ]
            [ Element.column []
                (List.map
                    (\person ->
                        Element.row []
                            [ radioButton []
                                { value = model.selectedPerson == Just person
                                , onChange =
                                    Just
                                        (\check ->
                                            if check then
                                                SelectPerson (Just person)

                                            else
                                                SelectPerson Nothing
                                        )
                                }
                            , Element.el
                                [ Element.Events.onClick
                                    (if model.selectedPerson /= Just person then
                                        SelectPerson (Just person)

                                     else
                                        SelectPerson Nothing
                                    )
                                ]
                                (Element.text (person.firstName ++ " " ++ person.lastName))
                            ]
                    )
                    persons
                )
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
            [ tabBar []
                { state = model.tabBarState
                , tabButtons = IconAndText [ ( "menu", "Main", MainTab ), ( "settings", "Settings / Preferences", SettingsTab ), ( "info", "Very large tab name that doesn't fit into the box", CustomTab ) ]
                , selectedTab = model.selectedTab
                , onTabSelect = TabBarSelect
                }
            ]
        , Element.row [ Element.width fill, spacing 16 ]
            [ tabBar []
                { state = model.tabBarState
                , tabButtons = TextOnly [ ( "Main", MainTab ), ( "Settings / Preferences", SettingsTab ), ( "Very large tab name that doesn't fit into the box", CustomTab ) ]
                , selectedTab = model.selectedTab
                , onTabSelect = TabBarSelect
                }
            ]
        , Element.row [ Element.width fill, spacing 16 ]
            [ tabBar []
                { state = model.tabBarState
                , tabButtons = IconOnly [ ( "menu", MainTab ), ( "settings", SettingsTab ), ( "info", CustomTab ) ]
                , selectedTab = model.selectedTab
                , onTabSelect = TabBarSelect
                }
            ]
        , Element.row [ Element.width fill, spacing 16 ]
            [ Element.el [ alignTop ] <|
                textField []
                    { id = FirstInputField
                    , label = "My input field"
                    , value = model.inputValue
                    , state = model.inputField
                    , onChange = Just (InputChange FirstInputField)
                    , onKeyDown = Just (Json.map InputKeyDown <| Json.field "key" string)
                    }
            , Element.el [ alignTop, Element.width fill ] <|
                textField [ width fill ]
                    { id = SecondInputField
                    , label = "Second field"
                    , value = model.secondInputValue
                    , state = model.inputField
                    , onChange =
                        if model.checkboxValue == Nothing || model.checkboxValue == Just False then
                            Nothing

                        else
                            Just (InputChange SecondInputField)
                    , onKeyDown = Nothing
                    }
            , Element.el [ alignTop ] <|
                textField [ password ]
                    { id = ThirdInputField
                    , label = "My password field"
                    , value = model.thirdInputValue
                    , state = model.inputField
                    , onChange = Just (InputChange ThirdInputField)
                    , onKeyDown = Nothing
                    }
            , textField
                [ withError
                    { default = "*Required"
                    , error =
                        if String.length model.errorInputValue < 4 then
                            Just "At least 4 characters are needed"

                        else
                            Nothing
                    }
                ]
                { id = ErrorInputField
                , label = "Error input field"
                , value = model.errorInputValue
                , state = model.inputField
                , onChange = Just (InputChange ErrorInputField)
                , onKeyDown = Nothing
                }
            ]
        , Element.row [ Element.width fill, spacing 16 ]
            [ textField [ multiline, height (px 100) ]
                { id = FirstMultiline
                , label = "My input field"
                , value = model.multilineValue
                , state = model.inputField
                , onChange = Just (InputChange FirstMultiline)
                , onKeyDown = Nothing
                }
            , textField [ multiline, height (px 100), width fill ]
                { id = SecondMultiline
                , label = "Second field"
                , value = model.secondMultilineValue
                , state = model.inputField
                , onChange =
                    if model.checkboxValue == Nothing || model.checkboxValue == Just False then
                        Nothing

                    else
                        Just (InputChange SecondMultiline)
                , onKeyDown = Nothing
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
                        Just (\_ -> NoOp)
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
                (Element.el [ Element.height shrink, Element.scrollbarX, Element.Border.width 1, Element.Border.color theme.colors.gray.light ]
                    (denseDataTable
                        [ DataTable.selection (\p -> Set.member p.id model.selectedPersons) (\p checked -> DataTableSelectionChange (Just p) checked) (\allSelected -> DataTableSelectionChange Nothing allSelected)
                        , DataTable.expansion (\p -> Set.member p.id model.expandedPersons) (\p expanded -> DataTableExpansionChange p expanded) (\p -> Element.text (Maybe.withDefault "Nothing" p.additionalInfo))
                        ]
                        { columns =
                            [ DataTable.column (Element.text "First name") (px 100) (\_ person -> Element.text person.firstName)
                            , DataTable.sortableColumn (Element.text "Last name") (px 100) (\_ person -> Element.text person.lastName) (List.sortBy (\p -> p.lastName))
                            , DataTable.sortableColumn (Element.text "Age") (px 100) (\_ person -> Element.text (String.fromInt person.age)) (List.sortBy (\p -> p.age))
                            ]
                        , items =
                            persons
                        , state = model.dataTable
                        }
                    )
                )
            , Element.el [ Element.height fill ]
                (Element.el [ Element.height shrink, Element.width (px 250), Element.scrollbarX, Element.Border.width 1, Element.Border.color theme.colors.gray.light ]
                    (dataTable
                        [ DataTable.selection (\p -> Set.member p.id model.selectedPersons) (\p checked -> DataTableSelectionChange (Just p) checked) (\allSelected -> DataTableSelectionChange Nothing allSelected)
                        , width shrink
                        ]
                        { columns =
                            [ DataTable.column (Element.text "First name") (px 100) (\_ person -> Element.text person.firstName)
                            , DataTable.sortableColumn (Element.text "Last name") (px 100) (\_ person -> Element.text person.lastName) (List.sortBy (\p -> p.lastName))
                            , DataTable.sortableColumn (Element.text "Age") (px 100) (\_ person -> Element.text (String.fromInt person.age)) (List.sortBy (\p -> p.age))
                            ]
                        , items =
                            persons
                        , state = model.dataTable
                        }
                    )
                )
            , Element.el [ Element.height fill, Element.width fill ]
                (Element.el [ Element.height shrink, Element.width fill, Element.Border.width 1, Element.Border.color theme.colors.gray.light ]
                    (dataTable
                        [ height (px 300)
                        , width fill
                        , infinite { loadingBottom = model.loadingBottom, loadingTop = model.loadingTop, loadExtraItems = \direction -> Just { loadCount = List.length persons, excessCount = 0, loadMsg = LoadPeople direction } }
                        , id "infinite-data-table"
                        , DataTable.rowColor
                            (\p ->
                                if p.age > 30 then
                                    Just (Element.rgb255 255 204 203)

                                else if p.age == 29 then
                                    Just (Element.rgb255 144 238 144)

                                else
                                    Nothing
                            )
                        ]
                        { columns =
                            [ DataTable.column (Element.text "First name") (fill |> Element.minimum 100) (\_ person -> Element.text person.firstName)
                            , DataTable.column (Element.text "Last name") (fill |> Element.minimum 100) (\_ person -> Element.text person.lastName)
                            , DataTable.externalSortableColumn (Element.text "Age") (fill |> Element.minimum 100) (\_ person -> Element.text (String.fromInt person.age)) DataTableSortChange
                            ]
                        , items =
                            model.infinitePersons
                        , state = model.dataTable
                        }
                    )
                )
            , Element.el [ Element.height fill, Element.width fill ]
                (Element.el [ Element.height shrink, Element.width fill, Element.Border.width 1, Element.Border.color theme.colors.gray.light ]
                    (denseDataTable
                        [ height (px 300)
                        , width fill
                        , infinite { loadingBottom = model.loadingBottom, loadingTop = model.loadingTop, loadExtraItems = \direction -> Just { loadCount = List.length persons, excessCount = 0, loadMsg = LoadPeople direction } }
                        , id "infinite-data-table2"
                        ]
                        { columns =
                            [ DataTable.column (Element.text "First name") (fill |> Element.minimum 100) (\_ person -> Element.text person.firstName)
                            , DataTable.column (Element.text "Last name") (fill |> Element.minimum 100) (\_ person -> Element.text person.lastName)
                            , DataTable.externalSortableColumn (Element.text "Age") (fill |> Element.minimum 100) (\_ person -> Element.text (String.fromInt person.age)) DataTableSortChange
                            ]
                        , items =
                            model.infinitePersons
                        , state = model.dataTable
                        }
                    )
                )
            , Element.column [ Element.height fill, Element.width fill, Element.Border.width 1, Element.Border.color theme.colors.gray.light ]
                [ dataTable
                    [ height (px 240), width fill ]
                    { columns =
                        [ DataTable.column (Element.text "First name") (fill |> Element.minimum 100) (\_ person -> Element.text person.firstName)
                        , DataTable.column (Element.text "Last name") (fill |> Element.minimum 100) (\_ person -> Element.text person.lastName)
                        , DataTable.externalSortableColumn (Element.text "Age") (fill |> Element.minimum 100) (\_ person -> Element.text (String.fromInt person.age)) DataTableSortChange
                        ]
                    , items =
                        model.persons
                    , state = model.dataTable
                    }
                , paginator []
                    { rowsPerPage = 4
                    , offset = 3
                    , total = 8
                    , nextPage = NoOp
                    , previousPage = NoOp
                    }
                ]
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
                        [ imageSrc (\person -> person.imageSrc)
                        , control (\person -> iconButton [ withTextColor (theme.colors.gray.withAlpha 0.46), IconButton.stopPropagation ] { icon = "delete", onPress = Just PressMinus })
                        ]
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
        , Element.row
            [ Element.spacing 24 ]
            [ Element.el [ Element.alignTop ]
                (card []
                    { media =
                        html
                            (Html.img [ Html.Attributes.style "width" "100%", Html.Attributes.style "height" "100%", Html.Attributes.style "object-fit" "cover", Html.Attributes.src "https://homepages.cae.wisc.edu/~ece533/images/goldhill.bmp" ] [])
                    , titles = { head = "MediaCenter With Header and Subheader", subHead = Just "Here is a very long subtitle that does not fit within the allotted space. The overflow of this is handled by an ellipsis." }
                    , thumbnail = Image "https://upload.wikimedia.org/wikipedia/commons/3/39/Lichtenstein_img_processing_test.png"
                    , content = Element.paragraph [ Element.paddingEach { left = 4, right = 4, top = 4, bottom = 4 } ] [ Element.text "This card is not expandable and it uses an image as the Thumbnail. It has some simple text content in a paragraph which does not require scrollbars to view in its entirety." ]
                    , actions =
                        [ ( "Button 1", SnackbarAlert (Plain "Snackbar message") )
                        , ( "Button 2", SnackbarAlert (WithAction "Snackbar message with action" "Repeat" (SnackbarAlert (Plain "Snackbar message"))) )
                        ]
                    , state = model.cardState
                    }
                )
            , Element.el [ Element.alignTop ]
                (card [ Card.expandable ]
                    { media =
                        html
                            (Html.img [ Html.Attributes.style "width" "100%", Html.Attributes.style "height" "100%", Html.Attributes.style "object-fit" "cover", Html.Attributes.src "https://homepages.cae.wisc.edu/~ece533/images/goldhill.bmp" ] [])
                    , titles = { head = "Header Only", subHead = Nothing }
                    , thumbnail = Image "https://upload.wikimedia.org/wikipedia/commons/3/39/Lichtenstein_img_processing_test.png"
                    , content =
                        Element.paragraph [ Element.paddingEach { left = 4, right = 4, top = 4, bottom = 4 } ] [ Element.text "This card is expandable and uses an image as the thumbnail. It has a horizontal scrollbar because all of the buttons do not fit within the card width." ]
                    , actions =
                        [ ( "Button 1", SnackbarAlert (Plain "Snackbar message") )
                        , ( "Button 2", SnackbarAlert (WithAction "Snackbar message with action" "Repeat" (SnackbarAlert (Plain "Snackbar message"))) )
                        , ( "Button 3", SnackbarAlert (Plain "Snackbar message") )
                        , ( "Button 4", SnackbarAlert (WithAction "Snackbar message with action" "Repeat" (SnackbarAlert (Plain "Snackbar message"))) )
                        , ( "Button 5", SnackbarAlert (Plain "Snackbar message") )
                        , ( "Button 6", SnackbarAlert (WithAction "Snackbar message with action" "Repeat" (SnackbarAlert (Plain "Snackbar message"))) )
                        ]
                    , state = model.cardState
                    }
                )
            , Element.el [ Element.alignTop ]
                (card [ Card.layout MediaTop, Card.expandable ]
                    { media =
                        html
                            (Html.img [ Html.Attributes.style "width" "100%", Html.Attributes.style "height" "100%", Html.Attributes.style "object-fit" "cover", Html.Attributes.src "https://homepages.cae.wisc.edu/~ece533/images/goldhill.bmp" ] [])
                    , titles = { head = "MediaTop", subHead = Just "With Subheader" }
                    , thumbnail = Icon "location_city"
                    , content = Element.el [ Element.paddingEach { left = 4, right = 4, top = 4, bottom = 4 } ] (Element.text "This card has the following:\n\n- Header\n\n- Subheader\n\n- Buttons\n\n- Content\n\nIt uses a different icon as the Thumbnail and has both a Header and Subheader.\nThe content of this card is too large for the provided display area. Therefore, horizontal and vertical scrollbars are present.")
                    , actions =
                        [ ( "Button 1", SnackbarAlert (Plain "Snackbar message") )
                        , ( "Button 2", SnackbarAlert (WithAction "Snackbar message with action" "Repeat" (SnackbarAlert (Plain "Snackbar message"))) )
                        ]
                    , state = model.cardState
                    }
                )
            , Element.el [ Element.alignTop ]
                (card [ Card.layout MediaTop, Card.expandable ]
                    { media =
                        html
                            (Html.img [ Html.Attributes.style "width" "100%", Html.Attributes.style "height" "100%", Html.Attributes.style "object-fit" "cover", Html.Attributes.src "https://homepages.cae.wisc.edu/~ece533/images/goldhill.bmp" ] [])
                    , titles = { head = "MediaTop Header Only", subHead = Nothing }
                    , thumbnail = Icon "settings"
                    , content = Element.paragraph [ Element.paddingEach { left = 4, right = 4, top = 4, bottom = 4 } ] [ Element.text "This card is expandable and does not have buttons." ]
                    , actions = []
                    , state = model.cardState
                    }
                )
            ]
        , Element.row [ Element.width fill, spacing 16 ]
            [ progressIndicator [ visibility model.progressIndicatorVisiblity1, circular ] { progress = model.progressIndicator1 }
            , progressIndicator [ visibility model.progressIndicatorVisiblity1, linear BottomUp ] { progress = model.progressIndicator1 }
            ]
        , Element.row [ Element.width fill, spacing 16 ]
            [ progressIndicator [ visibility model.progressIndicatorVisiblity1, circular, startDelaySeconds 0.5 ] { progress = model.progressIndicator2 }
            , progressIndicator [ visibility model.progressIndicatorVisiblity1, linear TopDown, startDelaySeconds 0.5 ] { progress = model.progressIndicator2 }
            ]
        , Element.row [ spacing 16 ]
            [ button [] { label = "Snackbar", onPress = Just (SnackbarAlert (Plain "Snackbar message")) }
            , button [] { label = "Snackbar With Action", onPress = Just (SnackbarAlert (WithAction "Snackbar message with action" "Alert Again" (SnackbarAlert (Plain "Tried again but failed")))) }
            ]
        , Element.el [ Element.height fill, Element.width fill, Element.centerX ]
            (snackbar [ Snackbar.dismissible ]
                { state = model.snackbarState
                }
            )
        , divider [ withColor theme.colors.primary.color ]
        , divider []
        ]
