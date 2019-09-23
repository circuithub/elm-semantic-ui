module SemanticUI.Modules.Dropdown exposing
    ( Config
    , DrawerState(..)
    , DropdownBuilder
    , ToggleEvent(..)
    , drawer
    , dropdown
    , init
    , item
    , readOnly
    , root
    , toggle
    , toggleEvent
    )

{-| Helper to create semantic-ui animated dropowns without the need for external JS.

As an example a big dropdown menu using layout:

    Dropdown.dropdown
        (Dropdown.init
            { identifier = "file"
            , onToggle = ToggleFile
            , state = model.file
            , layout =
                \({ toDropdown, toToggle, toDrawer, toItem } as outer) ->
                    toDropdown div
                        []
                        [ toToggle div [ class "text" ] [ text "File" ]
                        , toToggle i [ class "dropdown icon" ] []
                        , toDrawer div
                            []
                            [ toItem div [] [ text "New" ]
                            , toItem div [] [ span [ class "description" ] [ text "ctrl + o" ], text "Open..." ]
                            , toItem div [] [ span [ class "description" ] [ text "ctrl + s" ], text "Save as..." ]
                            , toItem div [] [ span [ class "description" ] [ text "ctrl + r" ], text "Rename" ]
                            , toItem div [] [ text "Make a copy" ]
                            , toItem div [] [ i [ class "folder icon" ] [], text "Move to folder" ]
                            , toItem div [] [ i [ class "trash icon" ] [], text "Move to trash" ]
                            , div [ class "divider" ] []
                            , toItem div [] [ text "Download As.." ]
                            , dropdown
                                (Dropdown.init
                                    { identifier = "fileSub"
                                    , onToggle = ToggleFileSub
                                    , layout =
                                        \{ toDropdown, toToggle, toDrawer, toItem } ->
                                            toDropdown (outer.toItem div)
                                                []
                                                [ toToggle div [] [ i [ class "dropdown icon" ] [], text "Publish to Web" ]
                                                , toDrawer div
                                                    []
                                                    [ toItem div [] [ text "Google Docs" ]
                                                    , toItem div [] [ text "Google Drive" ]
                                                    , toItem div [] [ text "Google Drive" ]
                                                    , toItem div [] [ text "Dropbox" ]
                                                    , toItem div [] [ text "Adobe Creative Cloud" ]
                                                    , toItem div [] [ text "Private FTP" ]
                                                    , toItem div [] [ text "Another Service..." ]
                                                    ]
                                                ]
                                    }
                                    |> Dropdown.toggleEvent Dropdown.OnHover
                                )
                                model.fileSub
                            ]
                        ]
            }
        )

As an example a big dropdown menu using primitives:

    let
        config =
            Dropdown.init { identifier = "file", onToggle = ToggleFile2, state = model.file2 }
    in
    Dropdown.root config
        div
        []
        [ Dropdown.toggle config div [ class "text" ] [ text "File" ]
        , Dropdown.toggle config i [ class "dropdown icon" ] []
        , Dropdown.drawer config
            div
            []
            [ Dropdown.item div [] [ text "New" ]
            , Dropdown.item div [] [ span [ class "description" ] [ text "ctrl + o" ], text "Open..." ]
            , Dropdown.item div [] [ span [ class "description" ] [ text "ctrl + s" ], text "Save as..." ]
            , Dropdown.item div [] [ span [ class "description" ] [ text "ctrl + r" ], text "Rename" ]
            , Dropdown.item div [] [ text "Make a copy" ]
            , Dropdown.item div [] [ i [ class "folder icon" ] [], text "Move to folder" ]
            , Dropdown.item div [] [ i [ class "trash icon" ] [], text "Move to trash" ]
            , div [ class "divider" ] []
            , Dropdown.item div [] [ text "Download As.." ]
            , let
                cfgSub =
                    Dropdown.init { identifier = "fileSub", onToggle = ToggleFileSub2 }
                        |> Dropdown.toggleEvent Dropdown.OnHover

                stSub =
                    model.fileSub2
              in
              Dropdown.root cfgSub
                stSub
                (Dropdown.item div)
                []
                [ Dropdown.toggle cfgSub stSub div [] [ i [ class "dropdown icon" ] [], text "Publish to Web" ]
                , Dropdown.drawer cfgSub
                    stSub
                    div
                    []
                    [ Dropdown.item div [] [ text "Google Docs" ]
                    , Dropdown.item div [] [ text "Google Drive" ]
                    , Dropdown.item div [] [ text "Google Drive" ]
                    , Dropdown.item div [] [ text "Dropbox" ]
                    , Dropdown.item div [] [ text "Adobe Creative Cloud" ]
                    , Dropdown.item div [] [ text "Private FTP" ]
                    , Dropdown.item div [] [ text "Another Service..." ]
                    ]
                ]
            ]
        ]

-}

import Dropdown
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import SemanticUI.Modules.HtmlBuilder as HtmlBuilder exposing (HtmlBuilder)


{-| Everything needed to build a particular dropdown.

A function that takes a record with the following functions

  - toDropdown - the function `root` with `Config` and `DrawerState` applied to it.
  - toToggle - the function `toggle` with `Config` and `DrawerState` applied to it.
  - toDrawer - the function `drawer` with `Config` and `DrawerState` applied to it.
  - toItem - the function `item` with `Config` and `DrawerState` applied to it.

The final resultant value is what you decide (some kind of DOM)

-}
type alias DropdownBuilder msg =
    { toDropdown : HtmlBuilder msg -> HtmlBuilder msg
    , toToggle : HtmlBuilder msg -> HtmlBuilder msg
    , toDrawer : HtmlBuilder msg -> HtmlBuilder msg
    , toItem : HtmlBuilder msg -> HtmlBuilder msg
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
type alias Config msg html =
    { drawerState : DrawerState
    , identifier : String
    , onToggle : DrawerState -> msg
    , toggleEvent : ToggleEvent
    , readOnly : Bool
    , layout : DropdownBuilder msg -> html
    }


{-| Create a default Config for a dropdown.

Takes:

  - The unique identifier.
  - The state change handler.

-}
init :
    { config | drawerState : DrawerState, identifier : String, onToggle : DrawerState -> msg, layout : DropdownBuilder msg -> html }
    -> Config msg html
init { drawerState, identifier, onToggle, layout } =
    { drawerState = drawerState
    , identifier = identifier
    , onToggle = onToggle
    , layout = layout
    , toggleEvent = OnClick
    , readOnly = False
    }


{-| Set toggleEvent on a Config
-}
toggleEvent : ToggleEvent -> { config | toggleEvent : ToggleEvent } -> { config | toggleEvent : ToggleEvent }
toggleEvent a config =
    { config | toggleEvent = a }


{-| Set readOnly on a Config
-}
readOnly : Bool -> { config | readOnly : Bool } -> { config | readOnly : Bool }
readOnly a config =
    { config | readOnly = a }


{-| The current state of the dropdown drawer
-}
type DrawerState
    = Closed
    | Opening
    | Opened
    | Closing


{-| Render a dropdown.

Takes:

  - Configuration
  - The current state of the dropdown
  - The layout function for the dropdown

-}
dropdown :
    Config msg html
    -> html
dropdown config =
    config.layout
        { toToggle = toggle config
        , toDrawer = drawer config
        , toItem = item
        , toDropdown = root config
        }


{-| Create the root node for a dropdown containing at least a toggle and a drawer

It takes the following:

  - `Config`
  - HTML renderer, typically passed in as a simple HTML element constructor e.g. `Html.div`
  - Node attributes
  - Node children, some of whitch were created using `toggle` and one using `drawer`

Amongst other things it adds the "ui dropdown" class to the root node.

-}
root :
    { config | drawerState : DrawerState, identifier : String, onToggle : DrawerState -> msg, toggleEvent : ToggleEvent, readOnly : Bool }
    -> HtmlBuilder msg
    -> HtmlBuilder msg
root config element attrs children =
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
        (toggle config
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


{-| Create an element for the toggle area that will toggle the drawer on toggle event.

It takes the following:

  - `Config`
  - HTML renderer, typically passed in as a simple HTML element constructor e.g. `Html.div`
  - Attributes of node
  - Children of node

-}
toggle :
    { config | drawerState : DrawerState, readOnly : Bool, onToggle : DrawerState -> msg, toggleEvent : ToggleEvent }
    -> HtmlBuilder msg
    -> HtmlBuilder msg
toggle config =
    if config.readOnly then
        identity

    else
        Dropdown.toggle
            { onToggle = toDropdownOnToggle config.drawerState config.onToggle
            , toggleEvent = toDropdownToggleEvent config.toggleEvent
            , isToggled = drawerIsOpen config.drawerState
            }


{-| Create the drawer element which is displayed when the dropdown is expanded/toggled.

It takes the following:

  - `Config`
  - HTML renderer, typically passed in as a simple HTML element constructor e.g. `Html.div`
  - Attributes of node
  - Children of node (some of which atleast created with `item`)

It adds the "menu" class to the node.

-}
drawer :
    { config | drawerState : DrawerState, readOnly : Bool, onToggle : DrawerState -> msg }
    -> HtmlBuilder msg
    -> HtmlBuilder msg
drawer ({ drawerState } as config) element =
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
        element
            |> HtmlBuilder.prependAttribute (classList [ ( "menu", True ) ])

    else
        Dropdown.drawer
            { drawerVisibleAttribute = classList []
            , isToggled = drawerIsOpen drawerState
            }
            element
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
item : HtmlBuilder msg -> HtmlBuilder msg
item =
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
