module SemanticUI.Modules.Dropdown.Selection exposing
    ( Builder
    , Config
    , Selection(..)
    , attributes
    , button
    , compact
    , disabled
    , dropdownIcon
    , fluid
    , inline
    , linkItem
    , scrolling
    , selection
    , single
    , toCustomHtml
    , toHtml
    , toItem
    , toggleEvent
    )

{-| A dropdown with an active selection state.

Example of `Selection.selection`:

    Selection.single
        { identifier = "yes-no-select"
        , onToggle = ToggleYesNoDrawer
        , onSelect = SetYesNo
        , drawerState = model.YesNoDrawerState
        , defaultLabel = text "Select yes/no"
        , selectionLabel = text << ynToString
        , currentSelection = model.yesNoSelection
        }
        |> Selection.toHtml
            { options = [ Yes, No ], optionLabel = text << toString }

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Builder as Html
import Html.Events exposing (..)
import SemanticUI.Elements.Button as Button
import SemanticUI.Modules.Dropdown as Dropdown
import SemanticUI.Modules.Dropdown.Drawer as Drawer
import SemanticUI.Modules.Dropdown.Select as Select exposing (Select(..), Variation)
import SemanticUI.Modules.Dropdown.Toggle as Toggle


{-| Most general configuration that applies any `Selection`.

It is recommended that you use `selection`, `inline`, `button`, `single` to construct this record.

-}
type alias Config msg option =
    { variation : Variation msg
    , drawerState : Drawer.State
    , identifier : String
    , onSelect : option -> msg
    , onToggle : Drawer.State -> msg
    , attributes : List (Attribute msg)
    , toggleEvent : Toggle.Event
    , disabled : Bool
    , optionAttributes : option -> List (Attribute msg)
    , labels : List (Html msg)
    , dropdownIcon : Bool
    , fluid : Bool
    , scrolling : Bool
    , formInput : Maybe { name : String, value : String }
    }


type Selection msg option
    = Selection (Config msg option)


{-| Everything needed to build the `Html msg` representation of a particular selection dropdown.
-}
type alias Builder msg option =
    Select.Builder msg option


{-| Any other custom `Attribute`s to add to the select. Custom attributes
will be added before `elm-semantic-ui` attributes.

Identical to `Dropdown.attributes`, and `Select.attributes`

-}
attributes : List (Attribute msg) -> Selection msg option -> Selection msg option
attributes a (Selection config) =
    Selection { config | attributes = a }


{-| Set `toggleEvent` on any `Selection`
-}
toggleEvent : Toggle.Event -> Selection msg option -> Selection msg option
toggleEvent a (Selection config) =
    Selection { config | toggleEvent = a }


{-| Set `disabled` on any `Selection`
-}
disabled : Bool -> Selection msg option -> Selection msg option
disabled a (Selection config) =
    Selection { config | disabled = a }


{-| Set `dropdownIcon` on a `Selection`
-}
dropdownIcon : Bool -> Selection msg option -> Selection msg option
dropdownIcon a (Selection config) =
    Selection { config | dropdownIcon = a }


{-| The dropdown will stretch horizontally to fill the space that it is in.
It may also contain floated content.

Identical to `Dropdown.fluid`, and `Select.fluid`

-}
fluid : Bool -> Selection msg option -> Selection msg option
fluid a (Selection config) =
    Selection { config | fluid = a }


{-| A scrolling dropdown can have its menu scroll.

Identical to `Dropdown.scrolling`, and `Select.scrolling`

-}
scrolling : Bool -> Selection msg option -> Selection msg option
scrolling a (Selection config) =
    Selection { config | scrolling = a }


{-| Reduce the space used by a select component.
A compact selection dropdown has no minimum width.
A compact button dropdown has reduced padding.
-}
compact : Bool -> Selection msg option -> Selection msg option
compact a (Selection config) =
    let
        variation =
            case config.variation of
                Select.Selection sel ->
                    Select.Selection { sel | compact = a }

                Select.Button but ->
                    Select.Button (Button.compact a but)

                _ ->
                    config.variation
    in
    Selection { config | variation = variation }


{-| A dropdown select component with a single active selection.
-}
single :
    { config
        | currentSelection : Maybe option
        , drawerState : Drawer.State
        , identifier : String
        , onToggle : Drawer.State -> msg
        , onSelect : option -> msg
        , defaultLabel : Html msg
        , selectionLabel : option -> Html msg
    }
    -> Selection msg option
single config =
    let
        (Select selectConfig) =
            Select.select
                { drawerState = config.drawerState
                , identifier = config.identifier
                , onToggle = config.onToggle
                , onSelect = config.onSelect
                , label =
                    Just
                        (span
                            [ classList [ ( "default", config.currentSelection == Nothing ), ( "text", True ) ]
                            , -- pointer-events: none ensures that an underlying input will be brought into focus, even with the label on top
                              -- this is helpful for search dropdowns, though search is not directly implemented as of yet
                              attribute "pointer-events" "none"
                            ]
                            [ case config.currentSelection of
                                Nothing ->
                                    config.defaultLabel

                                Just selectedOption ->
                                    config.selectionLabel selectedOption
                            ]
                        )
                }
    in
    Selection
        { variation = selectConfig.variation
        , drawerState = selectConfig.drawerState
        , identifier = selectConfig.identifier
        , onToggle = selectConfig.onToggle
        , onSelect = config.onSelect
        , attributes = selectConfig.attributes
        , toggleEvent = selectConfig.toggleEvent
        , disabled = selectConfig.disabled
        , dropdownIcon = selectConfig.dropdownIcon
        , labels = selectConfig.labels
        , fluid = selectConfig.fluid
        , scrolling = selectConfig.scrolling
        , formInput = Nothing
        , optionAttributes =
            \option ->
                [ classList [ ( "active selected", Just option == config.currentSelection ) ] ]
        }


{-| A single selection button dropdown that is labeled with the current selection.
-}
button :
    { config
        | button : Button.Config msg
        , currentSelection : Maybe option
        , drawerState : Drawer.State
        , identifier : String
        , onToggle : Drawer.State -> msg
        , onSelect : option -> msg
        , defaultLabel : Html msg
        , selectionLabel : option -> Html msg
    }
    -> Selection msg option
button config =
    let
        (Selection singleConfig) =
            single config
    in
    Selection { singleConfig | variation = Select.Button config.button }


{-| A single selection dropdown component that is styled similarly to a `<select>` form control.

This configuration can be used with an optional hidden `<input>` for forms.

-}
selection :
    { config
        | currentSelection : Maybe option
        , drawerState : Drawer.State
        , identifier : String
        , onToggle : Drawer.State -> msg
        , onSelect : option -> msg
        , formInput : Maybe { name : String, toValue : option -> String }
        , defaultLabel : Html msg
        , selectionLabel : option -> Html msg
    }
    -> Selection msg option
selection config =
    let
        (Selection singleConfig) =
            single config
    in
    Selection
        { singleConfig
            | variation = Select.Selection { compact = False }
            , formInput =
                config.formInput
                    |> Maybe.map
                        (\formInput ->
                            { name = formInput.name
                            , value =
                                config.currentSelection
                                    |> Maybe.map formInput.toValue
                                    |> Maybe.withDefault ""
                            }
                        )
            , dropdownIcon = True
        }


{-| A single selection dropdown component that can be used inline with text.
-}
inline :
    { config
        | currentSelection : Maybe option
        , drawerState : Drawer.State
        , identifier : String
        , onToggle : Drawer.State -> msg
        , onSelect : option -> msg
        , defaultLabel : Html msg
        , selectionLabel : option -> Html msg
    }
    -> Selection msg option
inline config =
    let
        (Selection singleConfig) =
            single config
    in
    Selection
        { singleConfig
            | variation = Select.Inline
            , dropdownIcon = True
        }


toHtml : { builder | optionLabel : option -> Html msg, options : List option } -> Selection msg option -> Html msg
toHtml { optionLabel, options } selectionControl =
    let
        layout { toDropdown, toOption, drawer } =
            let
                option val =
                    toOption val div [] [ optionLabel val ]
            in
            toDropdown div [] [ drawer [] (List.map option options) ]
    in
    toCustomHtml layout selectionControl


toCustomHtml : (Builder msg option -> Html msg) -> Selection msg option -> Html msg
toCustomHtml layout (Selection config) =
    let
        selectLayout { toDropdown, toToggle, toOption, drawer } =
            layout
                { toDropdown =
                    case config.formInput of
                        Nothing ->
                            toDropdown

                        Just formInput ->
                            \element ->
                                toDropdown element
                                    |> Html.appendChild
                                        (input [ hidden True, name formInput.name, value formInput.value ] [])
                , toToggle = toToggle
                , drawer = drawer
                , toOption = toOption
                }
    in
    Select
        { variation = config.variation
        , drawerState = config.drawerState
        , identifier = config.identifier
        , onToggle = config.onToggle
        , onSelect = config.onSelect
        , attributes = config.attributes
        , toggleEvent = config.toggleEvent
        , disabled = config.disabled
        , optionAttributes = config.optionAttributes
        , dropdownIcon = config.dropdownIcon
        , labels = config.labels
        , fluid = config.fluid
        , scrolling = config.scrolling
        }
        |> Select.toCustomHtml selectLayout


{-| Create an item that goes in the drawer.

Identical to `Dropdown.toItem`, and `Select.toItem`

-}
toItem : Html.Builder msg -> Html.Builder msg
toItem =
    Dropdown.toItem


{-| Create a menu item that goes in the drawer, formatted as if it were an `<a>` element.

Identical to `Dropdown.linkItem`, and `Select.linkItem`

-}
linkItem : List (Attribute msg) -> List (Html msg) -> Html msg
linkItem =
    Dropdown.linkItem
