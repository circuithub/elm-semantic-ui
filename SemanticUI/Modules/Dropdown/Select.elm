module SemanticUI.Modules.Dropdown.Select exposing (Config, DrawerState(..), dropdown)

{-| Refine a dropdown with selection state

Example of `Select.dropdown` :

    Select.dropdown
        (Select.init
            { identifier = "select1"
            , onToggle = ToggleSelect1
            , onSelect = SetSelect1
            , layout =
                \{ toDropdown, toDrawer, toOption, toToggle } ->
                    toDropdown div
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

Identical to `Dropdown.State`

-}
type DrawerState
    = Closed
    | Opening
    | Opened
    | Closing


{-| Define when the dropdown should open.

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
    { identifier : String
    , onSelect : option -> msg
    , onToggle : DrawerState -> msg
    , toggleEvent : ToggleEvent
    , readOnly : Bool
    , layout : DropdownBuilder msg html option -> html
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
type alias DropdownBuilder msg html option =
    { toDropdown : HtmlBuilder msg -> HtmlBuilder msg
    , toDrawer : HtmlBuilder msg -> HtmlBuilder msg
    , toToggle : HtmlBuilder msg -> HtmlBuilder msg
    , option : option -> html
    }


init :
    { config
        | identifier : String
        , onToggle : DrawerState -> msg
        , onSelect : option -> msg
        , layout : DropdownBuilder msg html option -> html
        , layoutOption : OptionBuilder msg option -> html
    }
    -> Config msg html option
init { identifier, onToggle, onSelect, layout, layoutOption } =
    { identifier = identifier
    , onToggle = onToggle
    , toggleEvent = OnClick
    , readOnly = False
    , onSelect = onSelect
    , layout = layout
    , layoutOption = layoutOption
    }


{-| A dropdown component with stateful selection.
-}
dropdown :
    Config msg html option
    -> DrawerState
    -> html
dropdown config state =
    Dropdown.dropdown
        { identifier = config.identifier
        , onToggle = config.onToggle << fromDropdownState
        , toggleEvent = toDropdownToggleEvent config.toggleEvent
        , readOnly = config.readOnly
        , layout =
            \{ toDropdown, toToggle, toDrawer, toItem } ->
                config.layout
                    { toDropdown = toDropdown
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
        (toDropdownState state)


toDropdownState : DrawerState -> Dropdown.State
toDropdownState state =
    case state of
        Opening ->
            Dropdown.Opening

        Closing ->
            Dropdown.Closing

        Opened ->
            Dropdown.Opened

        Closed ->
            Dropdown.Closed


fromDropdownState : Dropdown.State -> DrawerState
fromDropdownState state =
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
