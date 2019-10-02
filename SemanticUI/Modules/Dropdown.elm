module SemanticUI.Modules.Dropdown exposing
    ( Builder
    , Config
    , Dropdown(..)
    , Variation(..)
    , attributes
    , disabled
    , dropdown
    , dropdownIcon
    , fluid
    , linkItem
    , scrolling
    , toCustomHtml
    , toHtml
    , toItem
    , toggleEvent
    )

{-| Helper to create semantic-ui animated dropowns without the need for external JS.

As an example a big dropdown menu using layout:

    let
        mainMenu { toDropdown, toToggle, drawer } =
            toDropdown div
                []
                [ toToggle div [ class "text" ] [ text "File" ]
                , drawer
                    []
                    [ Dropdown.toItem div [] [ text "New" ]
                    , Dropdown.toItem div [] [ span [ class "description" ] [ text "ctrl + o" ], text "Open..." ]
                    , Dropdown.toItem div [] [ span [ class "description" ] [ text "ctrl + s" ], text "Save as..." ]
                    , Dropdown.toItem div [] [ span [ class "description" ] [ text "ctrl + r" ], text "Rename" ]
                    , Dropdown.toItem div [] [ text "Make a copy" ]
                    , Dropdown.toItem div [] [ i [ class "folder icon" ] [], text "Move to folder" ]
                    , Dropdown.toItem div [] [ i [ class "trash icon" ] [], text "Move to trash" ]
                    , div [ class "divider" ] []
                    , Dropdown.toItem div [] [ text "Download As.." ]
                    , Dropdown.dropdown
                        { identifier = "fileSub"
                        , onToggle = ToggleFileSub
                        , state = model.fileSub
                        }
                        |> Dropdown.toggleEvent Toggle.OnHover
                        |> Dropdown.dropdownIcon True
                        |> Dropdown.toCustomHtml subMenu
                    ]
                ]

        subMenu { toDropdown, toToggle, drawer } =
            toDropdown (Dropdown.toItem div)
                []
                [ toToggle div [] [ text "Publish to Web" ]
                , drawer
                    []
                    [ Dropdown.toItem div [] [ text "Google Docs" ]
                    , Dropdown.toItem div [] [ text "Google Drive" ]
                    , Dropdown.toItem div [] [ text "Google Drive" ]
                    , Dropdown.toItem div [] [ text "Dropbox" ]
                    , Dropdown.toItem div [] [ text "Adobe Creative Cloud" ]
                    , Dropdown.toItem div [] [ text "Private FTP" ]
                    , Dropdown.toItem div [] [ text "Another Service..." ]
                    ]
                ]
    in
    Dropdown.dropdown
        { identifier = "file"
        , onToggle = ToggleFile
        , state = model.file
        }
        |> Dropdown.dropdownIcon True
        |> Dropdown.toCustomHtml mainMenu

-}

import Dropdown
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import SemanticUI.Elements.Button as Button
import SemanticUI.Modules.Dropdown.Drawer as Drawer
import SemanticUI.Modules.Dropdown.Toggle as Toggle
import SemanticUI.Modules.HtmlBuilder as HtmlBuilder exposing (HtmlBuilder)


{-| Everything needed to build the `Html msg` representation of a particular dropdown.

  - toDropdown - converts a `<div>` or an `<a>` element into a SemanticUI dropdown
  - toToggle - converts an element into a toggle for the dropdown
  - drawer - the drawer element that can be toggled open or closed

-}
type alias Builder msg =
    { toDropdown : HtmlBuilder msg -> HtmlBuilder msg
    , toToggle : HtmlBuilder msg -> HtmlBuilder msg
    , drawer : HtmlBuilder msg
    }


{-| Dropdown variations. For internal use only.
-}
type Variation msg
    = Ordinary
    | Button (Button.Config msg)


{-| Configuration for a dropdown

  - drawerState - Current state of the dropdown
  - identifier - Unique identifier for the dropdown
  - onToggle - Handle state change of the dropdown
  - toggleEvent - When should the dropdown expand (default Toggle.OnClick)
  - disabled - Is the dropdown disabled (default False)
  - dropdownIcon - Whether a dropdown icon is visible (default False)

-}
type alias Config msg =
    { variation : Variation msg
    , uiClassList : List ( String, Bool )
    , drawerState : Drawer.State
    , identifier : String
    , onToggle : Drawer.State -> msg
    , attributes : List (Attribute msg)
    , toggleEvent : Toggle.Event
    , disabled : Bool
    , dropdownIcon : Bool
    , labels : List (Html msg)
    , fluid : Bool
    , scrolling : Bool
    }


{-| A type that represents the dropdown.

Use `dropdown` to construct it.

-}
type Dropdown msg
    = Dropdown (Config msg)


{-| Any other custom `Attribute`s to add to dropdown. Custom attributes
will be added before `elm-semantic-ui` attributes.
-}
attributes : List (Attribute msg) -> Dropdown msg -> Dropdown msg
attributes a (Dropdown config) =
    Dropdown { config | attributes = a }


{-| Set `toggleEvent` on a `Dropdown`
-}
toggleEvent : Toggle.Event -> Dropdown msg -> Dropdown msg
toggleEvent a (Dropdown config) =
    Dropdown { config | toggleEvent = a }


{-| Set `disabled` on a `Dropdown`
-}
disabled : Bool -> Dropdown msg -> Dropdown msg
disabled a (Dropdown config) =
    Dropdown { config | disabled = a }


{-| Set `dropdownIcon` on a `Dropdown`
-}
dropdownIcon : Bool -> Dropdown msg -> Dropdown msg
dropdownIcon a (Dropdown config) =
    Dropdown { config | dropdownIcon = a }


{-| The dropdown will stretch horizontally to fill the space that it is in.
It may also contain floated content.
-}
fluid : Bool -> Dropdown msg -> Dropdown msg
fluid a (Dropdown config) =
    Dropdown { config | fluid = a }


{-| A scrolling dropdown can have its menu scroll.
-}
scrolling : Bool -> Dropdown msg -> Dropdown msg
scrolling a (Dropdown config) =
    Dropdown { config | scrolling = a }


dropdown :
    { config | drawerState : Drawer.State, identifier : String, onToggle : Drawer.State -> msg, label : Maybe (Html msg) }
    -> Dropdown msg
dropdown config =
    Dropdown
        { variation = Ordinary
        , uiClassList = []
        , drawerState = config.drawerState
        , identifier = config.identifier
        , onToggle = config.onToggle
        , attributes = []
        , toggleEvent = Toggle.OnClick
        , disabled = False
        , dropdownIcon = False
        , labels = Maybe.withDefault [] (Maybe.map List.singleton config.label)
        , fluid = False
        , scrolling = False
        }


{-| A button dropdown variation
-}
button :
    { config | button : Button.Config msg, drawerState : Drawer.State, identifier : String, onToggle : Drawer.State -> msg, label : Maybe (Html msg) }
    -> Dropdown msg
button config =
    let
        (Dropdown ordinaryConfig) =
            dropdown config
    in
    Dropdown { ordinaryConfig | variation = Button config.button }


{-| Render a dropdown with the provided layout
-}
toCustomHtml : (Builder msg -> Html msg) -> Dropdown msg -> Html msg
toCustomHtml layout (Dropdown config) =
    let
        isVisible =
            Drawer.isVisible config.drawerState

        toDropdown =
            toRoot
                { uiClassList =
                    [ ( "active", isVisible )
                    , ( "visible", isVisible )
                    , ( "disabled", config.disabled )
                    , ( "fluid", config.fluid )
                    , ( "scrolling", config.scrolling )
                    ]
                        ++ config.uiClassList
                , drawerState = config.drawerState
                , identifier = config.identifier
                , onToggle = config.onToggle
                , toggleEvent = config.toggleEvent
                , dropdownIcon = config.dropdownIcon
                , labels = config.labels
                }
    in
    layout
        { toToggle = toToggle config
        , toDropdown =
            case config.variation of
                Ordinary ->
                    toToggle config << toDropdown

                Button but ->
                    let
                        toButton =
                            \element attrs children ->
                                Button.viewAs
                                    (element
                                        |> HtmlBuilder.prependAttributes attrs
                                    )
                                    but
                                    children
                    in
                    toToggle config << toDropdown << toButton
        , drawer = drawer config
        }


{-| Render a dropdown with a simple default layout
-}
toHtml : { builder | items : List (Html msg) } -> Dropdown msg -> Html msg
toHtml { items } dropdownControl =
    let
        layout builder =
            builder.toDropdown div [] [ builder.drawer [] (List.map (toItem div [] << List.singleton) items) ]
    in
    toCustomHtml layout dropdownControl


toRoot :
    { config
        | uiClassList : List ( String, Bool )
        , drawerState : Drawer.State
        , identifier : String
        , onToggle : Drawer.State -> msg
        , toggleEvent : Toggle.Event
        , labels : List (Html msg)
        , dropdownIcon : Bool
    }
    -> HtmlBuilder msg
    -> HtmlBuilder msg
toRoot config element attrs children =
    Dropdown.root
        { identifier = config.identifier
        , onToggle = toDropdownOnToggle config.drawerState config.onToggle
        , toggleEvent = toDropdownToggleEvent config.toggleEvent
        , isToggled = Drawer.isToggled config.drawerState
        }
        element
        (attrs
            ++ [ style "position" "relative"
               , class "ui"
               , classList config.uiClassList
               , class "dropdown"
               ]
        )
        (config.labels
            ++ (if config.dropdownIcon then
                    [ i [ class "dropdown icon" ] [] ]

                else
                    []
               )
            ++ children
        )


toToggle :
    { config | drawerState : Drawer.State, disabled : Bool, onToggle : Drawer.State -> msg, toggleEvent : Toggle.Event }
    -> HtmlBuilder msg
    -> HtmlBuilder msg
toToggle config =
    if config.disabled then
        identity

    else
        Dropdown.toggle
            { onToggle = toDropdownOnToggle config.drawerState config.onToggle
            , toggleEvent = toDropdownToggleEvent config.toggleEvent
            , isToggled = Drawer.isToggled config.drawerState
            }


drawer :
    { config | drawerState : Drawer.State, disabled : Bool, onToggle : Drawer.State -> msg }
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
drawer ({ drawerState } as config) =
    let
        isTransitioning =
            Drawer.isTransitioning drawerState

        isVisible =
            Drawer.isVisible drawerState

        finalState =
            case drawerState of
                Drawer.Opening ->
                    Drawer.Opened

                Drawer.Closing ->
                    Drawer.Closed

                _ ->
                    drawerState
    in
    if config.disabled then
        div
            |> HtmlBuilder.prependAttribute (classList [ ( "menu", True ) ])

    else
        Dropdown.drawer
            { drawerVisibleAttribute = classList []
            , isToggled = Drawer.isToggled drawerState
            }
            div
            |> HtmlBuilder.prependAttributes
                (classList
                    [ ( "menu", True )
                    , ( "active", True )
                    , ( "visible", isVisible )
                    , ( "hidden", not isVisible )
                    , ( "animating", isTransitioning )
                    , ( "transition", True )
                    , ( "slide", isTransitioning )
                    , ( "down", isTransitioning )
                    , ( "in", drawerState == Drawer.Opening )
                    , ( "out", drawerState == Drawer.Closing )
                    ]
                    :: (if isTransitioning then
                            [ on "animationend" (Decode.succeed (config.onToggle finalState)) ]

                        else
                            []
                       )
                )


{-| Create a menu item that goes in the drawer.
Converts a `<div>` or an `<a>` element into a SemanticUI dropdown item.

It adds the "item" class to the node.

-}
toItem : HtmlBuilder msg -> HtmlBuilder msg
toItem =
    HtmlBuilder.appendAttribute (class "item")


{-| Create a menu item that goes in the drawer, formatted as if it were an `<a>` element.

It adds the "link" and "item" classes to the node.

-}
linkItem : List (Attribute msg) -> List (Html msg) -> Html msg
linkItem =
    div
        |> HtmlBuilder.appendAttribute (class "link item")


toDropdownToggleEvent : Toggle.Event -> Dropdown.ToggleEvent
toDropdownToggleEvent event =
    case event of
        Toggle.OnClick ->
            Dropdown.OnClick

        Toggle.OnHover ->
            Dropdown.OnHover

        Toggle.OnFocus ->
            Dropdown.OnFocus


toDrawerState : Drawer.State -> Dropdown.State -> Drawer.State
toDrawerState currentState toggleOpen =
    case ( toggleOpen, Drawer.isToggled currentState ) of
        ( True, False ) ->
            Drawer.Opening

        ( False, True ) ->
            Drawer.Closing

        _ ->
            currentState


toDropdownOnToggle : Drawer.State -> (Drawer.State -> msg) -> Dropdown.State -> msg
toDropdownOnToggle currentState onToggle toggleOpen =
    onToggle (toDrawerState currentState toggleOpen)
