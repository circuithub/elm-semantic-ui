module SemanticUI.Modules.Dropdown exposing
    ( Config
    , DropdownBuilder
    , State(..)
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

    dropdown
        (Dropdown.init { identifier = "file", onToggle = ToggleFile })
        model.file
        (\({ toDropdown, toToggle, toDrawer, toItem } as outer) ->
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
                        (Dropdown.init { identifier = "fileSub", onToggle = ToggleFileSub }
                            |> Dropdown.toggleEvent Dropdown.OnHover
                        )
                        model.fileSub
                        (\{ toDropdown, toToggle, toDrawer, toItem } ->
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
                        )
                    ]
                ]
        )

As an example a big dropdown menu using primitives:

    let
        cfg =
            Dropdown.init { identifier = "file", onToggle = ToggleFile2 }

        st =
            model.file2
    in
    Dropdown.root cfg
        st
        div
        []
        [ Dropdown.toggle cfg st div [ class "text" ] [ text "File" ]
        , Dropdown.toggle cfg st i [ class "dropdown icon" ] []
        , Dropdown.drawer cfg
            st
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
import SemanticUI.Modules.Common exposing (HtmlBuilder)


{-| Everything needed to build a particular dropdown.

A function that takes a record with the following functions

  - toDropdown - the function `root` with `Config` and `State` applied to it.
  - toToggle - the function `toggle` with `Config` and `State` applied to it.
  - toDrawer - the function `drawer` with `Config` and `State` applied to it.
  - toItem - the function `item` with `Config` and `State` applied to it.

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

  - identifier - Unique identifier for the dropdown
  - onToggle - Handle state change of the dropdown
  - toggleEvent - When should the dropdown expand (default OnClick)
  - readOnly - Is the dropdown read only (default False)

-}
type alias Config msg =
    { identifier : String
    , onToggle : State -> msg
    , toggleEvent : ToggleEvent
    , readOnly : Bool
    }


{-| Create a default Config for a dropdown.

Takes:

  - The unique identifier.
  - The state change handler.

-}
init : { config | identifier : String, onToggle : State -> msg } -> Config msg
init { identifier, onToggle } =
    { identifier = identifier
    , onToggle = onToggle
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
type State
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
    Config msg
    -> State
    -> (DropdownBuilder msg -> html)
    -> html
dropdown cfg state layout =
    layout
        { toToggle = toggle cfg state
        , toDrawer = drawer cfg state
        , toItem = item
        , toDropdown = root cfg state
        }


{-| Create the root node for a dropdown containing at least a toggle and a drawer

It takes the following:

  - `Config`
  - `State`
  - HTML renderer, typically passed in as a simple HTML element constructor e.g. `Html.div`
  - Node attributes
  - Node children, some of whitch were created using `toggle` and one using `drawer`

Amongst other things it adds the "ui dropdown" class to the root node.

-}
root : Config msg -> State -> HtmlBuilder msg -> HtmlBuilder msg
root cfg state element attrs children =
    let
        isVisible =
            drawerIsVisible state
    in
    Dropdown.dropdown element
        (classList
            [ ( "ui", True )
            , ( "dropdown", True )
            , ( "active", isVisible )
            , ( "visible", isVisible )
            , ( "disabled", cfg.readOnly )
            ]
            :: style "position" "relative"
            :: attrs
        )
        (List.map (\e _ _ -> e)
            (toggle cfg
                state
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
        )
        (drawerIsOpen state)
        (toDropdownConfig cfg state)


{-| Create an element for the toggle area that will toggle the drawer on toggle event.

It takes the following:

  - `Config`
  - `State`
  - HTML renderer, typically passed in as a simple HTML element constructor e.g. `Html.div`
  - Attributes of node
  - Children of node

-}
toggle : Config msg -> State -> HtmlBuilder msg -> HtmlBuilder msg
toggle cfg state element attrs children =
    if cfg.readOnly then
        element attrs children

    else
        Dropdown.toggle element attrs children (drawerIsOpen state) (toDropdownConfig cfg state)


{-| Create the drawer element which is displayed when the dropdown is expanded/toggled.

It takes the following:

  - `Config`
  - `State`
  - HTML renderer, typically passed in as a simple HTML element constructor e.g. `Html.div`
  - Attributes of node
  - Children of node (some of which atleast created with `item`)

It adds the "menu" class to the node.

-}
drawer : Config msg -> State -> HtmlBuilder msg -> HtmlBuilder msg
drawer cfg state element attrs children =
    let
        isTransitioning =
            drawerIsTransitioning state

        isVisible =
            drawerIsVisible state

        finalState =
            case state of
                Opening ->
                    Opened

                Closing ->
                    Closed

                _ ->
                    state
    in
    if cfg.readOnly then
        element
            (classList
                [ ( "menu", True )
                ]
                :: attrs
            )
            children

    else
        Dropdown.drawer element
            ((classList
                [ ( "menu", True )
                , ( "active", True )
                , ( "visible", isVisible )
                , ( "hiden", not isVisible )
                , ( "animating", isTransitioning )
                , ( "transition", True )
                , ( "slide", isTransitioning )
                , ( "down", isTransitioning )
                , ( "in", state == Opening )
                , ( "out", state == Closing )
                ]
                :: (if isTransitioning then
                        [ on "animationend" (Decode.succeed (cfg.onToggle finalState)) ]

                    else
                        []
                   )
             )
                ++ attrs
            )
            children
            (drawerIsOpen state)
            (toDropdownConfig cfg state)


{-| Create an item that goes in the drawer.

It takes the following:

  - `Config`
  - `State`
  - HTML renderer, typically passed in as a simple HTML element constructor e.g. `Html.div`
  - Attributes of node
  - Children of node

It adds the "item" class to the node.

-}
item : HtmlBuilder msg -> HtmlBuilder msg
item element attrs children =
    element (class "item" :: attrs) children


drawerIsOpen : State -> Bool
drawerIsOpen state =
    state == Opening || state == Opened


drawerIsTransitioning : State -> Bool
drawerIsTransitioning state =
    state == Opening || state == Closing


drawerIsVisible : State -> Bool
drawerIsVisible state =
    state /= Closed


toDropdownConfig : Config msg -> State -> Dropdown.Config msg
toDropdownConfig cfg state =
    { identifier = cfg.identifier
    , toggleEvent =
        case cfg.toggleEvent of
            OnClick ->
                Dropdown.OnClick

            OnHover ->
                Dropdown.OnHover

            OnFocus ->
                Dropdown.OnFocus
    , drawerVisibleAttribute = classList []
    , callback =
        \toggleOpen ->
            let
                newState =
                    case ( toggleOpen, drawerIsOpen state ) of
                        ( True, False ) ->
                            Opening

                        ( False, True ) ->
                            Closing

                        _ ->
                            state
            in
            cfg.onToggle newState
    }
