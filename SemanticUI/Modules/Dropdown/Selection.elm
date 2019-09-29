module SemanticUI.Modules.Dropdown.Selection exposing
    ( Builder
    , Config
    , attributes
    , button
    , compact
    , disabled
    , dropdownIcon
    , fluid
    , inline
    , scrolling
    , selection
    , single
    , toCustomHtml
    , toHtml
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
import Html.Events exposing (..)
import SemanticUI.Elements.Button as Button
import SemanticUI.Modules.Dropdown as Dropdown
import SemanticUI.Modules.Dropdown.Drawer as Drawer
import SemanticUI.Modules.Dropdown.Select as Select exposing (Select(..), Variation(..))
import SemanticUI.Modules.Dropdown.Toggle as Toggle
import SemanticUI.Modules.HtmlBuilder as HtmlBuilder exposing (HtmlBuilder)


{-| Most general configuration that applies any `Selection`.

It is recommended that you use `selection`, `inline`, `button`, `single` to construct this record.

-}
type alias Config msg option =
    Select.Config msg option


type alias Selection msg option =
    Select msg option


{-| Everything needed to build the `Html msg` representation of a particular selection dropdown.
-}
type alias Builder msg option =
    Select.Builder msg option


{-| Any other custom `Attribute`s to add to the select. Custom attributes
will be added before `elm-semantic-ui` attributes.

Identical to `Dropdown.attributes`, and `Select.attributes`

-}
attributes : List (Attribute msg) -> Selection msg option -> Selection msg option
attributes =
    Select.attributes


{-| Set `toggleEvent` on any `Selection`
-}
toggleEvent : Toggle.Event -> Selection msg option -> Selection msg option
toggleEvent =
    Select.toggleEvent


{-| Set `disabled` on any `Selection`
-}
disabled : Bool -> Selection msg option -> Selection msg option
disabled =
    Select.disabled


{-| Set `dropdownIcon` on a `Selection`
-}
dropdownIcon : Bool -> Selection msg option -> Selection msg option
dropdownIcon =
    Select.dropdownIcon


{-| The dropdown will stretch horizontally to fill the space that it is in.
It may also contain floated content.

Identical to `Dropdown.fluid`, and `Select.fluid`

-}
fluid : Bool -> Selection msg option -> Selection msg option
fluid =
    Select.fluid


{-| A scrolling dropdown can have its menu scroll.

Identical to `Dropdown.scrolling`, and `Select.scrolling`

-}
scrolling : Bool -> Selection msg option -> Selection msg option
scrolling =
    Select.scrolling


{-| Reduce the space used by a select component.
A compact selection dropdown has no minimum width.
A compact button dropdown has reduced padding.
-}
compact : Bool -> Selection msg option -> Selection msg option
compact a (Select config) =
    let
        variation =
            case config.variation of
                Selection sel ->
                    Selection { sel | compact = a }

                Button but ->
                    Button (Button.compact a but)

                _ ->
                    config.variation
    in
    Select { config | variation = variation }


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
    Select
        { variation = Ordinary
        , drawerState = config.drawerState
        , identifier = config.identifier
        , onToggle = config.onToggle
        , onSelect = config.onSelect
        , formInput = Nothing
        , attributes = []
        , optionAttributes =
            \option ->
                [ classList [ ( "active selected", Just option == config.currentSelection ) ] ]
        , selectLabels =
            [ case config.currentSelection of
                Nothing ->
                    span [ class "default text" ] [ config.defaultLabel ]

                Just selectedOption ->
                    span [ class "text" ] [ config.selectionLabel selectedOption ]
            ]
        , toggleEvent = Toggle.OnClick
        , disabled = False
        , dropdownIcon = False
        , fluid = False
        , scrolling = False
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
        (Select singleConfig) =
            single config
    in
    Select { singleConfig | variation = Button config.button }


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
        (Select singleConfig) =
            single config
    in
    Select
        { singleConfig
            | variation = Selection { compact = False }
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
        (Select singleConfig) =
            single config
    in
    Select
        { singleConfig
            | variation = Inline
            , dropdownIcon = True
        }


toHtml : { builder | optionLabel : option -> Html msg, options : List option } -> Selection msg option -> Html msg
toHtml =
    Select.toHtml


toCustomHtml : (Builder msg option -> Html msg) -> Selection msg option -> Html msg
toCustomHtml =
    Select.toCustomHtml


{-| Create an item that goes in the drawer.

Identical to `Dropdown.toItem`, and `Select.toItem`

-}
toItem : HtmlBuilder msg -> HtmlBuilder msg
toItem =
    Dropdown.toItem


{-| Create a menu item that goes in the drawer, formatted as if it were an `<a>` element.

Identical to `Dropdown.linkItem`, and `Select.linkItem`

-}
linkItem : List (Attribute msg) -> List (Html msg) -> Html msg
linkItem =
    Dropdown.linkItem
