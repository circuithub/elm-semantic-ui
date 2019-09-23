module SemanticUI.Modules.Dropdown.Select exposing (Config, DrawerState(..), select)

{-| Refine a dropdown with selection state

Example of `Select.select` :

    let
        menu { toSelect, toDrawer, toOption, toToggle } =
            let
                option val =
                    toOption val div [ class "text" ] [ text (toString val) ]
            in
            toSelect div
                [ class "inline" ]
                [ toToggle div
                    [ classList [ ( "default", model.select1Selection == Nothing ), ( "text", True ) ] ]
                    [ text (Maybe.withDefault "Nothing selected" model.select1Selection)
                    , i [ class "dropdown icon" ] []
                    ]
                , toDrawer div
                    []
                    (List.map option [ Yes, No ])
                ]
    in
    Select.select
        (Select.init
            { identifier = "select1"
            , onToggle = ToggleSelect1
            , onSelect = SetSelect1
            , drawerState = model.select1DrawerState
            }
        )
        |> Select.toHtml menu

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import SemanticUI.Modules.Dropdown as Dropdown
import SemanticUI.Modules.HtmlBuilder as HtmlBuilder exposing (HtmlBuilder)


{-| The current state of the dropdown drawer.

Identical to `Dropdown.DrawerState`

-}
type DrawerState
    = Closed
    | Opening
    | Opened
    | Closing


{-| Define when the dropdown drawer should open.

Identical to `Dropdown.ToggleEvent`

-}
type ToggleEvent
    = OnClick
    | OnHover
    | OnFocus


{-| Configuration for a select

  - identifier - unique identifier
  - onToggle - message to handle dropdown state changes
  - toggleEvent - what casuse dropdown state change (default OnClick)
  - readOnly - is the selection read only or not
  - onSelect - messages to handle selection change (when clicking on option)
  - layout
  - layoutOption

-}
type alias Config msg option =
    { drawerState : DrawerState
    , identifier : String
    , onSelect : option -> msg
    , onToggle : DrawerState -> msg
    , toggleEvent : ToggleEvent
    , readOnly : Bool
    }


{-| Everything needed to build a particular Select.option
-}
type alias OptionBuilder msg option =
    { toItem : HtmlBuilder msg -> HtmlBuilder msg
    , value : option
    }


{-| Everything needed to build a particular Select.dropdown
-}
type alias SelectBuilder msg option =
    { toSelect : HtmlBuilder msg -> HtmlBuilder msg
    , toDrawer : HtmlBuilder msg -> HtmlBuilder msg
    , toToggle : HtmlBuilder msg -> HtmlBuilder msg
    , toOption : option -> HtmlBuilder msg -> HtmlBuilder msg
    }


init :
    { config
        | drawerState : DrawerState
        , identifier : String
        , onToggle : DrawerState -> msg
        , onSelect : option -> msg
    }
    -> Config msg option
init config =
    { drawerState = config.drawerState
    , identifier = config.identifier
    , onToggle = config.onToggle
    , onSelect = config.onSelect
    , toggleEvent = OnClick
    , readOnly = False
    }


type Select msg option
    = Select (Config msg option)


{-| A dropdown component with stateful selection.
-}
select :
    Config msg option
    -> Select msg option
select =
    Select


toHtml : (SelectBuilder msg option -> Html msg) -> Select msg option -> Html msg
toHtml layout (Select config) =
    let
        dropdownLayout { toDropdown, toToggle, toDrawer } =
            layout
                { toSelect = toDropdown
                , toToggle = toToggle
                , toDrawer = toDrawer << HtmlBuilder.prependAttribute (onClick (config.onToggle Closing))
                , toOption =
                    \value ->
                        Dropdown.toItem << HtmlBuilder.prependAttribute (onClick (config.onSelect value))
                }
    in
    Dropdown.dropdown
        { drawerState = toDropdownDrawerState config.drawerState
        , identifier = config.identifier
        , onToggle = config.onToggle << fromDropdownDrawerState
        , toggleEvent = toDropdownToggleEvent config.toggleEvent
        , readOnly = config.readOnly
        }
        |> Dropdown.toHtml dropdownLayout


toDropdownDrawerState : DrawerState -> Dropdown.DrawerState
toDropdownDrawerState state =
    case state of
        Opening ->
            Dropdown.Opening

        Closing ->
            Dropdown.Closing

        Opened ->
            Dropdown.Opened

        Closed ->
            Dropdown.Closed


fromDropdownDrawerState : Dropdown.DrawerState -> DrawerState
fromDropdownDrawerState state =
    case state of
        Dropdown.Opening ->
            Opening

        Dropdown.Closing ->
            Closing

        Dropdown.Opened ->
            Opened

        Dropdown.Closed ->
            Closed


toDropdownToggleEvent : ToggleEvent -> Dropdown.ToggleEvent
toDropdownToggleEvent event =
    case event of
        OnClick ->
            Dropdown.OnClick

        OnHover ->
            Dropdown.OnHover

        OnFocus ->
            Dropdown.OnFocus
