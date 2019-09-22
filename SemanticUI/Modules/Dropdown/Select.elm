module SemanticUI.Modules.Dropdown.Select exposing (Config, DrawerState(..), dropdown)

{-| Refine a dropdown with selection state

Example of `Select.dropdown` :

    Select.dropdown
        (Select.init
            { identifier = "select1"
            , onToggle = ToggleSelect1
            , onSelect = SetSelect1
            }
        )
        model.select1DrawerState
        (\{ toDropdown, toDrawer, toOption, toToggle } ->
            toDropdown div
                [ class "inline" ]
                [ toToggle div
                    [ classList [ ( "default", model.select1Selection == Nothing ), ( "text", True ) ] ]
                    [ text (Maybe.withDefault "Nothing selected" model.select1Selection)
                    , i [ class "dropdown icon" ] []
                    ]
                , toDrawer div
                    []
                    (List.map
                        (toOption { layout = \{ toItem, value } -> toItem div [ class "text" ] [ text (toString value) ] })
                        [ Yes, No ]
                    )
                ]
        )

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



-- {-| Configuration for a select
--
--   - identifier - unique identifier
--   - onToggle - message to handle dropdown state changes
--   - toggleEvent - what casuse dropdown state change (default OnClick)
--   - readOnly - is the selection read only or not
--   - onSelect - messages to handle selection change (when clicking on option)
--   - options - the list of possible options
--   - inline - should the selection be inline or not
--     (dropdown does not have "selection" class but "inline" and
--     "text" class is added to the unexpanded item)
--
-- -}
-- type alias Config option selection msg =
--     { identifier : String
--     , onToggle : Dropdown.State -> msg
--     , toggleEvent : Dropdown.ToggleEvent
--     , readOnly : Bool
--     , onSelect : selection -> msg
--     , options : List option
--     , inline : Bool
--     , makeSelectToggle : (HtmlBuilder msg -> HtmlBuilder msg) -> Html msg
--     }


type alias Config msg option =
    { identifier : String
    , onSelect : option -> msg
    , onToggle : DrawerState -> msg
    , toggleEvent : ToggleEvent
    , readOnly : Bool
    }


init :
    { config
        | identifier : String
        , onToggle : DrawerState -> msg
        , onSelect : option -> msg
    }
    -> Config msg option
init { identifier, onToggle, onSelect } =
    { identifier = identifier
    , onToggle = onToggle
    , toggleEvent = OnClick
    , readOnly = False
    , onSelect = onSelect
    }





{-| Everything needed to build a particular Select.option
-}
type alias OptionBuilder msg option =
    { layout :
        { toItem : HtmlBuilder msg -> HtmlBuilder msg
        , value : option
        }
        -> Html msg
    }


{-| Everything needed to build a particular Select.dropdown
-}
type alias DropdownBuilder msg option =
    { toDropdown : HtmlBuilder msg -> HtmlBuilder msg
    , toDrawer : HtmlBuilder msg -> HtmlBuilder msg
    , toOption : OptionBuilder msg option -> option -> Html msg
    , toToggle : HtmlBuilder msg -> HtmlBuilder msg
    }


{-| A dropdown component with stateful selection.
-}
dropdown :
    Config msg option
    -> DrawerState
    -> (DropdownBuilder msg option -> html)
    -> html
dropdown config state layout =
    Dropdown.dropdown
        { identifier = config.identifier
        , onToggle = config.onToggle << fromDropdownState
        , toggleEvent = toDropdownToggleEvent config.toggleEvent
        , readOnly = config.readOnly
        }
        (toDropdownState state)
        (\{ toDropdown, toToggle, toDrawer, toItem } ->
            layout
                { toDropdown = toDropdown
                , toToggle = toToggle
                , toDrawer = toDrawer << HtmlBuilder.prependAttribute (onClick (config.onToggle Closing))
                , toOption =
                    \optionBuilder value ->
                        optionBuilder.layout
                            { toItem = toItem << HtmlBuilder.prependAttribute (onClick (config.onSelect value))
                            , value = value
                            }
                }
        )


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
