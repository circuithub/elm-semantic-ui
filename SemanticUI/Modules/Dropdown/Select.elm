module SemanticUI.Modules.Dropdown.Select exposing (Config, State, init, inline, makeSelectToggle, select, selectMaybe)

{-| Refine a dropdown to act as a selection

Example of `select` :

    Select.select
        (Select.init
            { identifier = "select1"
            , onToggle = ToggleSelect1
            , onSelect = SetSelect1
            , options = [ Yes, No ]
            }
        )
        { dropdownState = model.select
        , selectedValue = model.select1Selection
        }
        div
        []
        (\{ toOption, value } ->
            toOption div [ class "text" ] [ text (toString value) ]
        )

Example of `selectMaybe` :

    Select.selectMaybe
        (Select.init
            { identifier = "select2"
            , onToggle = ToggleSelect2
            , onSelect = SetSelect2
            , options = [ Yes, No ]
            }
        )
        { dropdownState = model.select2
        , selectedValue = model.select2Selection
        }
        div
        []
        (\{ toEmptyOption } ->
            toEmptyOption div [ class "text default" ] [ text "None Selected" ]
        )
        (\{ toOption, value } ->
            toOption div [ class "text" ] [ text (toString value) ]
        )

Example of inline select :

    Select.select
        (Select.init
            { identifier = "inline1"
            , onToggle = ToggleInline1
            , onSelect = SelectInline1
            , options = [ "today", "this week", "this month" ]
            }
            |> Select.inline True
        )
        { dropdownState = model.select
        , selectedValue = model.select1Selection
        }
        div
        []
        (\{ toOption, isSelectionLabel, value } ->
            toOption div
                [ classList [ ( "active", not isSelectionLabel && model.inline1Selection == value ) ] ]
                [ text value ]
        )

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import SemanticUI.Modules.Common exposing (HtmlRenderer)
import SemanticUI.Modules.Dropdown as Dropdown exposing (dropdown)


{-| Configuration for a select

  - identifier - unique identifier
  - onToggle - message to handle dropdown state changes
  - toggleEvent - what casuse dropdown state change (default OnClick)
  - readOnly - is the selection read only or not
  - onSelect - messages to handle selection change (when clicking on option)
  - options - the list of possible options
  - inline - should the selection be inline or not
    (dropdown does not have "selection" class but "inline" and
    "text" class is added to the unexpanded item)

-}
type alias Config option selection msg =
    { identifier : String
    , onToggle : Dropdown.State -> msg
    , toggleEvent : Dropdown.ToggleEvent
    , readOnly : Bool
    , onSelect : selection -> msg
    , options : List option
    , inline : Bool
    , makeSelectToggle : (HtmlRenderer msg -> HtmlRenderer msg) -> Html msg
    }


{-| Create a default Config given, unique identifier, dropdown state change handler
selection handler and list of options
-}
init :
    { config
        | identifier : String
        , onToggle : Dropdown.State -> msg
        , onSelect : selection -> msg
        , options : List option
    }
    -> Config option selection msg
init { identifier, onToggle, onSelect, options } =
    { identifier = identifier
    , onToggle = onToggle
    , toggleEvent = Dropdown.OnClick
    , readOnly = False
    , onSelect = onSelect
    , options = options
    , inline = False
    , makeSelectToggle = \toToggle -> toToggle i [ class "dropdown icon" ] []
    }


{-| Modify a Config's makeSelectToggle value
-}
makeSelectToggle :
    ((HtmlRenderer msg -> HtmlRenderer msg) -> Html msg)
    -> { config | makeSelectToggle : (HtmlRenderer msg -> HtmlRenderer msg) -> Html msg }
    -> { config | makeSelectToggle : (HtmlRenderer msg -> HtmlRenderer msg) -> Html msg }
makeSelectToggle a config =
    { config | makeSelectToggle = a }


{-| Modify a Config's inline value
-}
inline : Bool -> { config | inline : Bool } -> { config | inline : Bool }
inline a config =
    { config | inline = a }


type alias State a =
    { dropdownState : Dropdown.State
    , selectedValue : a
    }


{-| Everything needed to build a particular option item in the dropdown drawer.

A function that takes a record with the following functions

  - toDropdown - the function `root` with `Config` and `State` applied to it.
  - toToggle - the function `toggle` with `Config` and `State` applied to it.
  - toDrawer - the function `drawer` with `Config` and `State` applied to it.
  - toItem - the function `item` with `Config` and `State` applied to it.

The final resultant value is what you decide (some kind of DOM)

-}
type alias OptionBuilder msg option =
    { toOption : HtmlRenderer msg -> HtmlRenderer msg
    , isSelectionLabel : Bool
    , value : option
    }


{-| Draw the select.

Takes the following:

  - Configuration,
  - the current dropdown state,
  - the currently selected value,
  - A renderer for the top level HTML container, typically passed in as a simple HTML element constructor e.g. `Html.div`
  - any extra attributes applied to the top level DOM node and
  - the option layout function

The layoutOption function is provided with

  - `toOption` - A builder for the individual options
  - `isSelectionLabel` - A bool that is `True` if the option item is part of the root dropdown control or `False` if it is in the list of options.
  - `value` - The option value.

When not inline then "selection" class is added to the top level node otherwise
"inline" class is added and "text" class is added when the option is not rendered
as part of the list.

-}
select :
    Config option option msg
    -> { state | dropdownState : Dropdown.State, selectedValue : option }
    -> HtmlRenderer msg
    -> List (Attribute msg)
    -> (OptionBuilder msg option -> Html msg)
    -> Html msg
select cfg state rootElement rootAttrs layoutOption =
    dropdown
        { identifier = cfg.identifier
        , onToggle = cfg.onToggle
        , toggleEvent = cfg.toggleEvent
        , readOnly = cfg.readOnly
        }
        state.dropdownState
        (\{ toDropdown, toToggle, toDrawer, toItem } ->
            toDropdown rootElement
                (classList [ ( "selection", not cfg.inline ), ( "inline", cfg.inline ) ] :: rootAttrs)
                [ toToggle input [ type_ "hidden" ] []
                , layoutOption
                    { toOption =
                        \optionElement optionAttrs optionChildren ->
                            toToggle optionElement
                                (classList [ ( "text", cfg.inline ) ] :: optionAttrs)
                                optionChildren
                    , isSelectionLabel = True
                    , value = state.selectedValue
                    }
                , cfg.makeSelectToggle toToggle
                , toDrawer div
                    [ onClick (cfg.onToggle Dropdown.Closing) ]
                    (List.map
                        (\option ->
                            layoutOption
                                { toOption =
                                    \optionElement optionAttrs optionChildren ->
                                        toItem optionElement
                                            (onClick (cfg.onSelect option) :: optionAttrs)
                                            optionChildren
                                , isSelectionLabel = False
                                , value = option
                                }
                        )
                        cfg.options
                    )
                ]
        )


{-| Draw the select where there may not be a valid initial selection.

Takes the following:

  - Configuration,
  - the current dropdown state,
  - the currently selected value,
  - the type of top level DOM node as a render function,
  - any extra attributes applied to the top level DOM node,
  - the layout function when there is no selection and
  - the option layout function

The no selection layout function returns a DOM node and takes:

  - `toEmptyOption` - A builder for the empty selection label

The option layout function returns a DOM node and takes:

  - `toOption` - A builder for the individual options
  - `isSelectionLabel` - A bool that is `True` if the option item is part of the root dropdown control or `False` if it is in the list of options
  - `value` - The option value

-}
selectMaybe :
    Config option (Maybe option) msg
    -> { state | dropdownState : Dropdown.State, selectedValue : Maybe option }
    -> HtmlRenderer msg
    -> List (Attribute msg)
    -> ({ toEmptyOption : HtmlRenderer msg -> HtmlRenderer msg } -> Html msg)
    -> (OptionBuilder msg option -> Html msg)
    -> Html msg
selectMaybe cfg state rootElement rootAttrs layoutEmptySelect layoutOption =
    select
        { identifier = cfg.identifier
        , onToggle = cfg.onToggle
        , toggleEvent = cfg.toggleEvent
        , readOnly = cfg.readOnly
        , onSelect = cfg.onSelect
        , options = List.map Just cfg.options
        , inline = cfg.inline
        , makeSelectToggle = cfg.makeSelectToggle
        }
        state
        rootElement
        rootAttrs
        (\{ toOption, isSelectionLabel, value } ->
            case value of
                Nothing ->
                    layoutEmptySelect { toEmptyOption = toOption }

                Just option ->
                    layoutOption { toOption = toOption, isSelectionLabel = isSelectionLabel, value = option }
        )
