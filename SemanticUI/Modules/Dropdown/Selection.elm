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
    , label
    , linkItem
    , scrolling
    , selection
    , single
    , toCustomHtml
    , toHtml
    , toItem
    , toggleEvent
    , valueLabel
    , valueLabelWithDefault
    )

{-| A dropdown with an active selection state.

Example of `Selection.selection`:

    Selection.single
        { identifier = "yes-no-select"
        , onToggle = ToggleYesNoDrawer
        , onSelect = SetYesNo
        , drawerState = model.YesNoDrawerState
        , value = model.yesNoSelection
        }
        |> Selection.valueLabel = Ok << text << ynToString
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
type alias Config msg option selection =
    { variation : Variation msg
    , drawerState : Drawer.State
    , identifier : String
    , onSelect : option -> msg
    , onToggle : Drawer.State -> msg
    , attributes : List (Attribute msg)
    , toggleEvent : Toggle.Event
    , disabled : Bool
    , optionAttributes : option -> List (Attribute msg)
    , value : selection
    , labels : List (Html msg)
    , dropdownIcon : Bool
    , fluid : Bool
    , scrolling : Bool
    , formInput : Maybe { name : String, value : String }
    }


type Selection msg option selection
    = Selection (Config msg option selection)


{-| Everything needed to build the `Html msg` representation of a particular selection dropdown.
-}
type alias Builder msg option =
    Select.Builder msg option


{-| Any other custom `Attribute`s to add to the select. Custom attributes
will be added before `elm-semantic-ui` attributes.

Identical to `Dropdown.attributes`, and `Select.attributes`

-}
attributes : List (Attribute msg) -> Selection msg option selection -> Selection msg option selection
attributes a (Selection config) =
    Selection { config | attributes = a }


{-| Set `toggleEvent` on any `Selection`
-}
toggleEvent : Toggle.Event -> Selection msg option selection -> Selection msg option selection
toggleEvent a (Selection config) =
    Selection { config | toggleEvent = a }


{-| Set `disabled` on any `Selection`
-}
disabled : Bool -> Selection msg option selection -> Selection msg option selection
disabled a (Selection config) =
    Selection { config | disabled = a }


{-| Set `dropdownIcon` on a `Selection`
-}
dropdownIcon : Bool -> Selection msg option selection -> Selection msg option selection
dropdownIcon a (Selection config) =
    Selection { config | dropdownIcon = a }


{-| The dropdown will stretch horizontally to fill the space that it is in.
It may also contain floated content.

Identical to `Dropdown.fluid`, and `Select.fluid`

-}
fluid : Bool -> Selection msg option selection -> Selection msg option selection
fluid a (Selection config) =
    Selection { config | fluid = a }


{-| A scrolling dropdown can have its menu scroll.

Identical to `Dropdown.scrolling`, and `Select.scrolling`

-}
scrolling : Bool -> Selection msg option selection -> Selection msg option selection
scrolling a (Selection config) =
    Selection { config | scrolling = a }


{-| Reduce the space used by a select component.
A compact selection dropdown has no minimum width.
A compact button dropdown has reduced padding.
-}
compact : Bool -> Selection msg option selection -> Selection msg option selection
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


{-| Sets a label on the selection control.
Note that this creates a completely free-form label without any SemanitcUI classes.
Use `valueLabel` or `valueLabelWithDefault` for labels that reflect the current selection.
-}
label : Maybe (Html msg) -> Selection msg option selection -> Selection msg option selection
label a (Selection config) =
    Selection { config | labels = Maybe.withDefault [] (Maybe.map List.singleton a) }


{-| Generate a label for the current selection.
If the current selection is not a selected option, return `Err` to generate a default label.
-}
valueLabel : (selection -> Result (Html msg) (Html msg)) -> Selection msg option selection -> Selection msg option selection
valueLabel a ((Selection config) as sel) =
    let
        ( isPlaceholder, labelHtml ) =
            case a config.value of
                Err html ->
                    ( True, html )

                Ok html ->
                    ( False, html )
    in
    sel
        |> label
            (Just
                (span
                    [ classList [ ( "default", isPlaceholder ), ( "text", True ) ]
                    , -- pointer-events: none ensures that an underlying input will be brought into focus, even with the label on top
                      -- this is helpful for search dropdowns, though search is not directly implemented as of yet
                      attribute "pointer-events" "none"
                    ]
                    [ labelHtml ]
                )
            )


{-| Generate a label for the selection values wrapped in `Maybe`.
Supports the common use-case where no value is selected and a default placeholder label needs to be supplied.
-}
valueLabelWithDefault : Html msg -> (selection -> Html msg) -> Selection msg option (Maybe selection) -> Selection msg option (Maybe selection)
valueLabelWithDefault defaultHtml valueToHtml =
    valueLabel (Result.fromMaybe defaultHtml << Maybe.map valueToHtml)


{-| A dropdown select component with a single active selection.
-}
single :
    { config
        | value : selection
        , isSelected : option -> Bool
        , drawerState : Drawer.State
        , identifier : String
        , onToggle : Drawer.State -> msg
        , onSelect : option -> msg
    }
    -> Selection msg option selection
single config =
    let
        (Select selectConfig) =
            Select.select
                { drawerState = config.drawerState
                , identifier = config.identifier
                , onToggle = config.onToggle
                , onSelect = config.onSelect
                , label = Nothing
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
        , value = config.value
        , formInput = Nothing
        , optionAttributes =
            \option ->
                [ classList [ ( "active selected", config.isSelected option ) ] ]
        }


{-| A single selection button dropdown that is labeled with the current selection.
-}
button :
    { config
        | button : Button.Config msg
        , value : selection
        , drawerState : Drawer.State
        , identifier : String
        , onToggle : Drawer.State -> msg
        , onSelect : option -> msg
        , isSelected : option -> Bool
    }
    -> Selection msg option selection
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
        | value : selection
        , drawerState : Drawer.State
        , identifier : String
        , onToggle : Drawer.State -> msg
        , onSelect : option -> msg
        , isSelected : option -> Bool
        , formInput : Maybe { name : String, toValue : selection -> String }
    }
    -> Selection msg option selection
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
                            , value = formInput.toValue config.value
                            }
                        )
            , dropdownIcon = True
        }


{-| A single selection dropdown component that can be used inline with text.
-}
inline :
    { config
        | value : selection
        , drawerState : Drawer.State
        , identifier : String
        , onToggle : Drawer.State -> msg
        , onSelect : option -> msg
        , isSelected : option -> Bool
    }
    -> Selection msg option selection
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


toHtml : { builder | optionLabel : option -> Html msg, options : List option } -> Selection msg option selection -> Html msg
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


toCustomHtml : (Builder msg option -> Html msg) -> Selection msg option selection -> Html msg
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
