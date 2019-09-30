module SemanticUI.Modules.Dropdown.Select exposing
    ( Builder
    , Config
    , Select(..)
    , Variation(..)
    , attributes
    , button
    , disabled
    , dropdownIcon
    , fluid
    , inline
    , scrolling
    , select
    , toCustomHtml
    , toHtml
    , toggleEvent
    )

{-| A dropdown with selectable options in its drawer.

Example of `Select.select` :

    Select.select
        { identifier = "nav-select"
        , onToggle = ToggleNavSelect
        , onSelect = identity
        , drawerState = model.navDrawerState
        }
        |> Select.toHtml
            { options = [ NavHome, NavBack ]
            , optionLabel = text << navToString
            }

See also `SematicUI.Modules.Dropdown.Selection` for dropdowns that indicate a current selection state.

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import SemanticUI.Elements.Button as Button
import SemanticUI.Modules.Dropdown as Dropdown exposing (Dropdown(..))
import SemanticUI.Modules.Dropdown.Drawer as Drawer
import SemanticUI.Modules.Dropdown.Toggle as Toggle
import SemanticUI.Modules.HtmlBuilder as HtmlBuilder exposing (HtmlBuilder)


{-| Select variations. For internal use only.

Similar to `Dropdown.Variation`

-}
type Variation msg
    = Ordinary
    | Button (Button.Config msg)
    | Inline
    | Selection { compact : Bool }


{-| Most general configuration that applies any `Select`.

It is recommended that you use `inline`, `button`, or `select` to construct this record.

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
    }


type Select msg option
    = Select (Config msg option)


{-| Any other custom `Attribute`s to add to the select. Custom attributes
will be added before `elm-semantic-ui` attributes.

Identical to `Dropdown.attributes`

-}
attributes : List (Attribute msg) -> Select msg option -> Select msg option
attributes a (Select config) =
    Select { config | attributes = a }


{-| Set `toggleEvent` on any `Select`
-}
toggleEvent : Toggle.Event -> Select msg option -> Select msg option
toggleEvent a (Select config) =
    Select { config | toggleEvent = a }


{-| Set `disabled` on any `Select`
-}
disabled : Bool -> Select msg option -> Select msg option
disabled a (Select config) =
    Select { config | disabled = a }


{-| Set `dropdownIcon` on a `Select`
-}
dropdownIcon : Bool -> Select msg option -> Select msg option
dropdownIcon a (Select config) =
    Select { config | dropdownIcon = a }


{-| The dropdown will stretch horizontally to fill the space that it is in.
It may also contain floated content.

Identical to `Dropdown.fluid`

-}
fluid : Bool -> Select msg option -> Select msg option
fluid a (Select config) =
    Select { config | fluid = a }


{-| A scrolling dropdown can have its menu scroll.

Identical to `Dropdown.scrolling`

-}
scrolling : Bool -> Select msg option -> Select msg option
scrolling a (Select config) =
    Select { config | scrolling = a }


{-| Everything needed to build the `Html msg` representation of a particular select dropdown.

  - toDropdown - converts a `<div>` or an `<a>` element into a SemanticUI dropdown
  - toToggle - converts an element into a toggle for the dropdown
  - toOption - takes the option value and converts a `<div>` or an `<a>` element into a selectable option item
  - drawer - the drawer element that can be toggled open or closed

-}
type alias Builder msg option =
    { toDropdown : HtmlBuilder msg -> HtmlBuilder msg
    , toToggle : HtmlBuilder msg -> HtmlBuilder msg
    , toOption : option -> HtmlBuilder msg -> HtmlBuilder msg
    , drawer : HtmlBuilder msg
    }


{-| A dropdown component with selectable options.

A vanilla select component, which can be extended in many different ways (including multiple selection).
It does not highlight the current selection automatically, see `SemanticUI.Modules.Dropdown.Selection` for this.

-}
select :
    { config
        | drawerState : Drawer.State
        , identifier : String
        , onToggle : Drawer.State -> msg
        , onSelect : option -> msg
        , label : Maybe (Html msg)
    }
    -> Select msg option
select config =
    let
        (Dropdown dropdownConfig) =
            Dropdown.dropdown config
    in
    Select
        { variation = Ordinary
        , drawerState = dropdownConfig.drawerState
        , identifier = dropdownConfig.identifier
        , onToggle = dropdownConfig.onToggle
        , onSelect = config.onSelect
        , attributes = dropdownConfig.attributes
        , toggleEvent = dropdownConfig.toggleEvent
        , disabled = dropdownConfig.disabled
        , dropdownIcon = dropdownConfig.dropdownIcon
        , labels = dropdownConfig.labels
        , fluid = dropdownConfig.fluid
        , scrolling = dropdownConfig.scrolling
        , optionAttributes = \_ -> []
        }


{-| A button dropdown select
-}
button :
    { config
        | button : Button.Config msg
        , drawerState : Drawer.State
        , identifier : String
        , onToggle : Drawer.State -> msg
        , onSelect : option -> msg
        , label : Maybe (Html msg)
    }
    -> Select msg option
button config =
    let
        (Select ordinaryConfig) =
            select config
    in
    Select { ordinaryConfig | variation = Button config.button }


{-| A dropdown select component that can be used inline with text.
-}
inline :
    { config
        | drawerState : Drawer.State
        , identifier : String
        , onToggle : Drawer.State -> msg
        , onSelect : option -> msg
        , label : Maybe (Html msg)
    }
    -> Select msg option
inline config =
    let
        (Select ordinaryConfig) =
            select config
    in
    Select { ordinaryConfig | variation = Inline }


toHtml : { builder | optionLabel : option -> Html msg, options : List option } -> Select msg option -> Html msg
toHtml { optionLabel, options } selectControl =
    let
        layout { toDropdown, toOption, drawer } =
            let
                option val =
                    toOption val div [] [ optionLabel val ]
            in
            toDropdown div [] [ drawer [] (List.map option options) ]
    in
    toCustomHtml layout selectControl


toCustomHtml : (Builder msg option -> Html msg) -> Select msg option -> Html msg
toCustomHtml layout (Select config) =
    let
        dropdownLayout { toDropdown, toToggle, drawer } =
            layout
                { toDropdown =
                    \element ->
                        toDropdown element
                            |> HtmlBuilder.appendAttributes
                                [ case config.variation of
                                    Ordinary ->
                                        classList []

                                    Button _ ->
                                        classList []

                                    Inline ->
                                        class "inline"

                                    Selection sel ->
                                        classList [ ( "compact", sel.compact ), ( "selection", True ) ]
                                ]
                , toToggle = toToggle
                , drawer =
                    drawer
                        |> HtmlBuilder.prependAttribute (onClick (config.onToggle Drawer.Closing))
                , toOption =
                    \value ->
                        toItem << HtmlBuilder.prependAttributes (onClick (config.onSelect value) :: config.optionAttributes value)
                }
    in
    Dropdown
        { variation =
            case config.variation of
                Button but ->
                    Dropdown.Button but

                _ ->
                    Dropdown.Ordinary
        , drawerState = config.drawerState
        , identifier = config.identifier
        , onToggle = config.onToggle
        , attributes = config.attributes
        , toggleEvent = config.toggleEvent
        , disabled = config.disabled
        , dropdownIcon = config.dropdownIcon
        , labels = config.labels
        , fluid = config.fluid
        , scrolling = config.scrolling
        }
        |> Dropdown.toCustomHtml dropdownLayout


{-| Create an item that goes in the drawer.

Identical to `Dropdown.toItem`

-}
toItem : HtmlBuilder msg -> HtmlBuilder msg
toItem =
    Dropdown.toItem


{-| Create a menu item that goes in the drawer, formatted as if it were an `<a>` element.

Identical to `Dropdown.linkItem`

-}
linkItem : List (Attribute msg) -> List (Html msg) -> Html msg
linkItem =
    Dropdown.linkItem
