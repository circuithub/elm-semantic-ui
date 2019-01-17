module SemanticUI.Modules.Dropdown.Select exposing (Config, State, init, inline, select, selectMaybe)

{-| Refine a dropdown to act as a selection

Example of `select` :

    Select.select
        (Select.init { identifier = "select1", onToggle = ToggleSelect1, onSelect = SetSelect1, choices =  [ Yes, No ]})
        { dropdownState = model.select1, selectedValue = model.select1Selection }
        div
        []
        (\makeSelect _ a -> makeSelect div [ class "text" ] [ text (toString a) ])

Example of `selectMaybe` :

    Select.selectMaybe
        (Select.init { identifier = "select2", onToggle = ToggleSelect2, onSelect = SetSelect2, choices =  [ Yes, No ]})
        { dropdownState = model.select2, selectedValue = model.select2Selection }
        div
        []
        (\makeNoSelect -> makeNoSelect div [ class "text default" ] [ text "None Selected" ])
        (\makeSelect _ a -> makeSelect div [ class "text" ] [ text (toString a) ])

Example of inline select :

    Select.select
        (Select.init { identifier = "inline1", onToggle =  ToggleInline1, onSelect = SelectInline1, choices = [ "today", "this week", "this month" ]} |> Select.inline True)
        model.inline1
        model.inline1Selection
        div
        []
        (\makeChoice isList c ->
            makeChoice div
                [ classList [ ( "active", isList && model.inline1Selection == c ) ] ]
                [ text c ]
        )

-}

import SemanticUI.Modules.Common exposing (Render, WrappedNode)
import SemanticUI.Modules.Dropdown as Dropdown exposing (dropdown)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


{-| Configuration for a select

  - identifier - unique identifier
  - onToggle - message to handle dropdown state changes
  - toggleEvent - what casuse dropdown state change (default OnClick)
  - readOnly - is the selection read only or not
  - onSelect - messages to handle selection change (when clicking on choice)
  - choices - the list of possible choices
  - inline - should the selection be inline or not
    (dropdown does not have "selection" class but "inline" and
    "text" class is added to the unexpanded item)

-}
type alias Config r c s msg =
    { r
        | identifier : String
        , onToggle : Dropdown.State -> msg
        , toggleEvent : Dropdown.ToggleEvent
        , readOnly : Bool
        , onSelect : s -> msg
        , choices : List c
        , inline : Bool
    }


{-| Create a default Config given, unique identifier, dropdown state change handler
selection handler and list of choices
-}
init :
    { r
        | identifier : String
        , onToggle : Dropdown.State -> msg
        , onSelect : s -> msg
        , choices : List c
    }
    -> Config {} c s msg
init { identifier, onToggle, onSelect, choices } =
    { identifier = identifier
    , onToggle = onToggle
    , toggleEvent = Dropdown.OnClick
    , readOnly = False
    , onSelect = onSelect
    , choices = choices
    , inline = False
    }


{-| Modify a Configuration's inline value
-}
inline : Bool -> { a | inline : Bool } -> { a | inline : Bool }
inline b a =
    { a | inline = b }


type alias State r a =
    { r
        | dropdownState : Dropdown.State
        , selectedValue : a
    }


{-| Draw the select.

Takes the following:

  - Configuration,
  - the current dropdown state,
  - the currently selected value,
  - the type of top level DOM node as a render function,
  - any extra attributes applied to the top level DOM node and
  - the choice layout function

The choice layout function returns a DOM node and takes:

  - A wrapped node (give it a node render function attributes and children).
  - A bool that is true choice being rendered is part of the list and false if
    its the selected choice.
  - The choice value.

When not inline then "selection" class is added to the top level node otherwise
"inline" calsss is added and "text" class is added when the choice is not rendered
as part of the list.

-}
select :
    Config r0 a a msg
    -> State r1 a
    -> Render msg
    -> List (Attribute msg)
    -> (WrappedNode msg -> Bool -> a -> Html msg)
    -> Html msg
select cfg st renderRoot rootAttrs layoutChoice =
    dropdown
        { identifier = cfg.identifier
        , onToggle = cfg.onToggle
        , toggleEvent = cfg.toggleEvent
        , readOnly = cfg.readOnly
        }
        st.dropdownState
        (\{ makeDropdown, makeToggle, makeDrawer, makeItem } ->
            makeDropdown renderRoot
                (classList [ ( "selection", not cfg.inline ), ( "inline", cfg.inline ) ] :: rootAttrs)
                [ makeToggle input [ type_ "hidden" ] []
                , layoutChoice (\r a c -> makeToggle r (classList [ ( "text", cfg.inline ) ] :: a) c) False st.selectedValue
                , makeToggle i [ class "dropdown icon" ] []
                , makeDrawer div
                    [ onClick (cfg.onToggle Dropdown.Closing) ]
                    (List.map
                        (\k ->
                            layoutChoice (makeItem << (\r a c -> r (onClick (cfg.onSelect k) :: a) c)) True k
                        )
                        cfg.choices
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
  - the choice layout function

The no selection layout function returns a DOM node and takes:

  - A wrapped node (give it a node render function attributes and children).

The choice layout function returns a DOM node and takes:

  - A wrapped node (give it a node render function attributes and children).
  - A bool that is true choice being rendered is part of the list and false if
    its the selected choice.
  - The choice value.

-}
selectMaybe :
    Config r0 a (Maybe a) msg
    -> State r1 (Maybe a)
    -> Render msg
    -> List (Attribute msg)
    -> (WrappedNode msg -> Html msg)
    -> (WrappedNode msg -> Bool -> a -> Html msg)
    -> Html msg
selectMaybe cfg st renderRoot rootAttrs layoutNone layout =
    let
        renderChoice_ =
            \w b ma ->
                case ma of
                    Nothing ->
                        layoutNone w

                    Just a ->
                        layout w b a
    in
    select
        { identifier = cfg.identifier
        , onToggle = cfg.onToggle
        , toggleEvent = cfg.toggleEvent
        , readOnly = cfg.readOnly
        , onSelect = cfg.onSelect
        , choices = List.map Just cfg.choices
        , inline = cfg.inline
        }
        st
        renderRoot
        rootAttrs
        renderChoice_
