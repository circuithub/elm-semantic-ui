module SemanticUI.Modules.Dropdown exposing
    ( Builder
    , Config
    , DrawerState(..)
    , Dropdown(..)
    , ToggleEvent(..)
    , dropdown
    , readOnly
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
                , toToggle i [ class "dropdown icon" ] []
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
                        |> Dropdown.toggleEvent Dropdown.OnHover
                        |> Dropdown.toHtml subMenu
                    ]
                ]

        subMenu { toDropdown, toToggle, drawer } =
            toDropdown (Dropdown.toItem div)
                []
                [ toToggle div [] [ i [ class "dropdown icon" ] [], text "Publish to Web" ]
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
        |> Dropdown.toHtml mainMenu

-}

import Dropdown
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
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


{-| Define when the dropdown should open
-}
type ToggleEvent
    = OnClick
    | OnHover
    | OnFocus


{-| Configuration for a dropdown

  - drawerState - Current state of the dropdown
  - identifier - Unique identifier for the dropdown
  - onToggle - Handle state change of the dropdown
  - toggleEvent - When should the dropdown expand (default OnClick)
  - readOnly - Is the dropdown read only (default False)

-}
type alias Config msg =
    { drawerState : DrawerState
    , identifier : String
    , onToggle : DrawerState -> msg
    , toggleEvent : ToggleEvent
    , readOnly : Bool
    }


{-| A type that represents the dropdown.

Use `dropdown` to construct it.

-}
type Dropdown msg
    = Dropdown (Config msg)


{-| Set `toggleEvent` on a `Dropdown`
-}
toggleEvent : ToggleEvent -> Dropdown msg -> Dropdown msg
toggleEvent a (Dropdown config) =
    Dropdown { config | toggleEvent = a }


{-| Set `readOnly` on a `Dropdown`
-}
readOnly : Bool -> Dropdown msg -> Dropdown msg
readOnly a (Dropdown config) =
    Dropdown { config | readOnly = a }


{-| The current state of the dropdown drawer
-}
type DrawerState
    = Closed
    | Opening
    | Opened
    | Closing


dropdown :
    { config | drawerState : DrawerState, identifier : String, onToggle : DrawerState -> msg }
    -> Dropdown msg
dropdown config =
    Dropdown
        { drawerState = config.drawerState
        , identifier = config.identifier
        , onToggle = config.onToggle
        , toggleEvent = OnClick
        , readOnly = False
        }


{-| Render a dropdown with the provided layout
-}
toHtml : (Builder msg -> Html msg) -> Dropdown msg -> Html msg
toHtml layout (Dropdown config) =
    layout
        { toToggle = toToggle config
        , toDropdown = toRoot config
        , drawer = drawer config
        }

toRoot :
    { config | drawerState : DrawerState, identifier : String, onToggle : DrawerState -> msg, toggleEvent : ToggleEvent, readOnly : Bool }
    -> HtmlBuilder msg
    -> HtmlBuilder msg
toRoot config element attrs children =
    let
        isVisible =
            drawerIsVisible config.drawerState
    in
    Dropdown.root
        { identifier = config.identifier
        , onToggle = toDropdownOnToggle config.drawerState config.onToggle
        , toggleEvent = toDropdownToggleEvent config.toggleEvent
        , isToggled = drawerIsOpen config.drawerState
        }
        element
        (classList
            [ ( "ui", True )
            , ( "dropdown", True )
            , ( "active", isVisible )
            , ( "visible", isVisible )
            , ( "disabled", config.readOnly )
            ]
            :: style "position" "relative"
            :: attrs
        )
        (toToggle config
            div
            [ style "position" "absolute"
            , style "width" "100%"
            , style "height" "100%"
            , style "left" "0%"
            , style "top" "0%"
            ]
            []
            :: children
        )


toToggle :
    { config | drawerState : DrawerState, readOnly : Bool, onToggle : DrawerState -> msg, toggleEvent : ToggleEvent }
    -> HtmlBuilder msg
    -> HtmlBuilder msg
toToggle config =
    if config.readOnly then
        identity

    else
        Dropdown.toggle
            { onToggle = toDropdownOnToggle config.drawerState config.onToggle
            , toggleEvent = toDropdownToggleEvent config.toggleEvent
            , isToggled = drawerIsOpen config.drawerState
            }


drawer :
    { config | drawerState : DrawerState, readOnly : Bool, onToggle : DrawerState -> msg }
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
drawer ({ drawerState } as config) =
    let
        isTransitioning =
            drawerIsTransitioning drawerState

        isVisible =
            drawerIsVisible drawerState

        finalState =
            case drawerState of
                Opening ->
                    Opened

                Closing ->
                    Closed

                _ ->
                    drawerState
    in
    if config.readOnly then
        div
            |> HtmlBuilder.prependAttribute (classList [ ( "menu", True ) ])

    else
        Dropdown.drawer
            { drawerVisibleAttribute = classList []
            , isToggled = drawerIsOpen drawerState
            }
            div
            |> HtmlBuilder.prependAttributes
                (classList
                    [ ( "menu", True )
                    , ( "active", True )
                    , ( "visible", isVisible )
                    , ( "hiden", not isVisible )
                    , ( "animating", isTransitioning )
                    , ( "transition", True )
                    , ( "slide", isTransitioning )
                    , ( "down", isTransitioning )
                    , ( "in", drawerState == Opening )
                    , ( "out", drawerState == Closing )
                    ]
                    :: (if isTransitioning then
                            [ on "animationend" (Decode.succeed (config.onToggle finalState)) ]

                        else
                            []
                       )
                )


{-| Create an item that goes in the drawer.

It adds the "item" class to the node.

-}
toItem : HtmlBuilder msg -> HtmlBuilder msg
toItem =
    HtmlBuilder.prependAttribute (class "item")


drawerIsOpen : DrawerState -> Bool
drawerIsOpen state =
    state == Opening || state == Opened


drawerIsTransitioning : DrawerState -> Bool
drawerIsTransitioning state =
    state == Opening || state == Closing


drawerIsVisible : DrawerState -> Bool
drawerIsVisible state =
    state /= Closed


toDropdownToggleEvent : ToggleEvent -> Dropdown.ToggleEvent
toDropdownToggleEvent event =
    case event of
        OnClick ->
            Dropdown.OnClick

        OnHover ->
            Dropdown.OnHover

        OnFocus ->
            Dropdown.OnFocus


toDrawerState : DrawerState -> Dropdown.State -> DrawerState
toDrawerState currentState toggleOpen =
    case ( toggleOpen, drawerIsOpen currentState ) of
        ( True, False ) ->
            Opening

        ( False, True ) ->
            Closing

        _ ->
            currentState


toDropdownOnToggle : DrawerState -> (DrawerState -> msg) -> Dropdown.State -> msg
toDropdownOnToggle currentState onToggle toggleOpen =
    onToggle (toDrawerState currentState toggleOpen)
