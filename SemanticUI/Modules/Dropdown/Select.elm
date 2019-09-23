module SemanticUI.Modules.Dropdown.Select exposing (Config, DrawerState(..), select)

{-| Refine a dropdown with selection state

Example of `Select.select` :

    Select.select
        (Select.init
            { identifier = "select1"
            , onToggle = ToggleSelect1
            , onSelect = SetSelect1
            , layout =
                \{ toSelect, toDrawer, toOption, toToggle } ->
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
            , layoutOption =
                \{ toItem, value } ->
                    toItem div [ class "text" ] [ text (toString value) ]
            }
        )
        model.select1DrawerState

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
type alias Config msg html option =
    { drawerState : DrawerState
    , identifier : String
    , onSelect : option -> msg
    , onToggle : DrawerState -> msg
    , toggleEvent : ToggleEvent
    , readOnly : Bool
    , layout : SelectBuilder msg html option -> html
    , layoutOption : OptionBuilder msg option -> html
    }


{-| Everything needed to build a particular Select.option
-}
type alias OptionBuilder msg option =
    { toItem : HtmlBuilder msg -> HtmlBuilder msg
    , value : option
    }


{-| Everything needed to build a particular Select.dropdown
-}
type alias SelectBuilder msg html option =
    { toSelect : HtmlBuilder msg -> HtmlBuilder msg
    , toDrawer : HtmlBuilder msg -> HtmlBuilder msg
    , toToggle : HtmlBuilder msg -> HtmlBuilder msg
    , option : option -> html
    }


init :
    { config
        | drawerState : DrawerState
        , identifier : String
        , onToggle : DrawerState -> msg
        , onSelect : option -> msg
        , layout : SelectBuilder msg html option -> html
        , layoutOption : OptionBuilder msg option -> html
    }
    -> Config msg html option
init config =
    { drawerState = config.drawerState
    , identifier = config.identifier
    , onToggle = config.onToggle
    , onSelect = config.onSelect
    , layout = config.layout
    , layoutOption = config.layoutOption
    , toggleEvent = OnClick
    , readOnly = False
    }


{-| A dropdown component with stateful selection.
-}
select :
    Config msg html option
    -> html
select config =
    Dropdown.dropdown
        { drawerState = toDropdownDrawerState config.drawerState
        , identifier = config.identifier
        , onToggle = config.onToggle << fromDropdownDrawerState
        , toggleEvent = toDropdownToggleEvent config.toggleEvent
        , readOnly = config.readOnly
        , layout =
            \{ toDropdown, toToggle, toDrawer, toItem } ->
                config.layout
                    { toSelect = toDropdown
                    , toToggle = toToggle
                    , toDrawer = toDrawer << HtmlBuilder.prependAttribute (onClick (config.onToggle Closing))
                    , option =
                        \value ->
                            config.layoutOption
                                { toItem = toItem << HtmlBuilder.prependAttribute (onClick (config.onSelect value))
                                , value = value
                                }
                    }
        }


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
