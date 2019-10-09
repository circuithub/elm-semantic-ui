module SemanticUI.Modules.Dropdown exposing
    ( Builder
    , Config
    , Dropdown(..)
    , Variation(..)
    , attributes
    , caret
    , disabled
    , dropdown
    , fluid
    , linkItem
    , scrolling
    , toCustomHtml
    , toHtml
    , toItem
    , toggleEvent
    )

{-| Provides a free-form [dropdown](https://semantic-ui.com/modules/dropdown.html) Semantic UI component.

Use this module to create dropdowns that have drawers that contain mostly custom components like checkboxes / radio buttons etc. For more typical dropdowns with selectable menu items see `SemanticUI.Modules.Dropdown.Select` or `SemanticUI.Modules.Dropdown.Selection`.

Example of two dropdown menus in a nested layout configuration:

    let
        mainMenu { toDropdown, drawer } =
            toDropdown div
                []
                [ div [ class "text" ] [ text "File" ]
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
                        |> Dropdown.caret True
                        |> Dropdown.toCustomHtml subMenu
                    ]
                ]

        subMenu { toDropdown, drawer } =
            toDropdown (Dropdown.toItem div)
                []
                [ div [] [ text "Publish to Web" ]
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
        |> Dropdown.caret True
        |> Dropdown.toCustomHtml mainMenu

-}

import Dropdown
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Builder as Html
import Html.Events exposing (..)
import Json.Decode as Decode
import SemanticUI.Elements.Button as Button
import SemanticUI.Modules.Dropdown.Drawer as Drawer
import SemanticUI.Modules.Dropdown.Toggle as Toggle


{-| Everything needed to build the `Html msg` representation of a particular dropdown.

  - toDropdown - converts a `<div>` or an `<a>` element into a SemanticUI dropdown
  - drawer - the drawer element that can be toggled open or closed

-}
type alias Builder msg =
    { toDropdown : Html.Builder msg -> Html.Builder msg
    , drawer : Html.Builder msg
    }


{-| Dropdown variations. For internal use only.
-}
type Variation msg
    = Ordinary
    | Button (Button.Config msg)


{-| Most general configuration that applies any `Dropdown`.

It is recommended that you use `dropdown` or `button` to properly initialize this config.

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
    , caret : Bool
    , labels : List (Html msg)
    , fluid : Bool
    , scrolling : Bool
    }


{-| A type that represents the dropdown.

Use `dropdown` or `button` to construct it and `toHtml` or `toCustomHtml` to render it.

-}
type Dropdown msg
    = Dropdown (Config msg)


{-| Any other custom `Attribute`s to add to dropdown. Custom attributes
will be added before `elm-semantic-ui` attributes.
-}
attributes : List (Attribute msg) -> Dropdown msg -> Dropdown msg
attributes a (Dropdown config) =
    Dropdown { config | attributes = a }


{-| The toggle event indicates when the dropdown should expand (default `Toggle.OnClick`)
-}
toggleEvent : Toggle.Event -> Dropdown msg -> Dropdown msg
toggleEvent a (Dropdown config) =
    Dropdown { config | toggleEvent = a }


{-| Disabled dropdowns cannot be toggled and indicates a read-only or inactive state. (default `False`)
-}
disabled : Bool -> Dropdown msg -> Dropdown msg
disabled a (Dropdown config) =
    Dropdown { config | disabled = a }


{-| A caret icon may be rendered inside the dropdown toggle (default value depends on the type of dropdown being constructed).
-}
caret : Bool -> Dropdown msg -> Dropdown msg
caret a (Dropdown config) =
    Dropdown { config | caret = a }


{-| The dropdown will stretch horizontally to fill the space that it is in.
It may also contain floated content.
-}
fluid : Bool -> Dropdown msg -> Dropdown msg
fluid a (Dropdown config) =
    Dropdown { config | fluid = a }


{-| A scrolling dropdown can have its menu scroll (default `False`).
-}
scrolling : Bool -> Dropdown msg -> Dropdown msg
scrolling a (Dropdown config) =
    Dropdown { config | scrolling = a }


{-| Construct a dropdown component. Render it with `toHtml` or `toCustomHtml`.

  - drawerState - Current state of the dropdown drawer
  - identifier - Unique identifier for the dropdown
  - onToggle - A state change in the dropdown drawer
  - label - The label rendered inside the dropdown toggle component

-}
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
        , caret = False
        , labels = Maybe.withDefault [] (Maybe.map List.singleton config.label)
        , fluid = False
        , scrolling = False
        }


{-| A button variation of the basic `dropdown` component.

  - button - Button configuration and customization

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


{-| Render a dropdown with the provided layout. Use this when `toHtml` doesn't offer enough flexibility.
Use `toItem` and `linkItem` to apply Semantic UI styles to items in the drawer.

  - layout - A function that is supplied with a dropdown `Builder`. Use the supplied builder functions to layout the dropdown however you wish!

-}
toCustomHtml : (Builder msg -> Html msg) -> Dropdown msg -> Html msg
toCustomHtml layout (Dropdown config) =
    let
        isVisible =
            Drawer.isVisible config.drawerState

        toDropdown element =
            element
                |> toRoot
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
                    , caret = config.caret
                    }
                |> toToggle config
                |> Html.prependAttributes config.attributes
    in
    layout
        { toDropdown =
            case config.variation of
                Ordinary ->
                    toDropdown << Html.prependChildren config.labels

                Button but ->
                    let
                        toButton =
                            \element attrs children ->
                                Button.viewAs
                                    (\buttonAttrs buttonChildren ->
                                        element (attrs ++ buttonAttrs) (buttonChildren ++ children)
                                    )
                                    but
                                    config.labels
                    in
                    toDropdown << toButton
        , drawer = drawer config
        }


{-| Render a dropdown with a simple default layout

  - items - A list of items to display in the drawer (typically supplied as simple `Html.text` nodes)

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
        , caret : Bool
    }
    -> Html.Builder msg
    -> Html.Builder msg
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
        ((if config.caret then
            [ i [ class "dropdown icon" ] [] ]

          else
            []
         )
            ++ children
        )


toToggle :
    { config | drawerState : Drawer.State, disabled : Bool, onToggle : Drawer.State -> msg, toggleEvent : Toggle.Event }
    -> Html.Builder msg
    -> Html.Builder msg
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
            |> Html.prependAttribute (classList [ ( "menu", True ) ])

    else
        Dropdown.drawer
            { drawerVisibleAttribute = classList []
            , isToggled = Drawer.isToggled drawerState
            }
            div
            |> Html.prependAttributes
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
toItem : Html.Builder msg -> Html.Builder msg
toItem =
    Html.appendAttribute (class "item")


{-| Create a menu item that goes in the drawer, formatted as if it were an `<a>` element.

It adds the "link" and "item" classes to the node.

-}
linkItem : List (Attribute msg) -> List (Html msg) -> Html msg
linkItem =
    div
        |> Html.appendAttribute (class "link item")


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
