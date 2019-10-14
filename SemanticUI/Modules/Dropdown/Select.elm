module SemanticUI.Modules.Dropdown.Select exposing
    ( Builder
    , Config
    , Select(..)
    , Variation(..)
    , attributes
    , button
    , caret
    , disabled
    , fluid
    , inline
    , linkItem
    , scrolling
    , select
    , toCustomHtml
    , toHtml
    , toItem
    , toggleEvent
    )

{-| A [dropdown](https://semantic-ui.com/modules/dropdown.html) with selectable options in its drawer.

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

See `SematicUI.Modules.Dropdown.Selection` for dropdowns that indicate a current selection state.

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Builder as Html
import Html.Events exposing (..)
import SemanticUI.Elements.Button as Button
import SemanticUI.Modules.Dropdown as Dropdown exposing (Dropdown(..))
import SemanticUI.Modules.Dropdown.Drawer as Drawer
import SemanticUI.Modules.Dropdown.Toggle as Toggle


{-| Select variations. For internal use only.

Similar to `Dropdown.Variation`

-}
type Variation msg
    = Ordinary
    | Button (Button.Config msg)
    | Inline
    | Selection { compact : Bool }


variationClassList : Variation msg -> List ( String, Bool )
variationClassList variation =
    case variation of
        Ordinary ->
            []

        Button _ ->
            []

        Inline ->
            [ ( "inline", True ) ]

        Selection { compact } ->
            [ ( "compact", compact ), ( "selection", True ) ]


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
    , caret : Bool
    , fluid : Bool
    , scrolling : Bool
    }


{-| A type that represents the dropdown select.

Use `inline`, `button`, or `select` to construct this record and `toHtml` or `toCustomHtml` to render it.

-}
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

Identical to `Dropdown.toggleEvent`

-}
toggleEvent : Toggle.Event -> Select msg option -> Select msg option
toggleEvent a (Select config) =
    Select { config | toggleEvent = a }


{-| Set `disabled` on any `Select`.

Identical to `Dropdown.disabled`

-}
disabled : Bool -> Select msg option -> Select msg option
disabled a (Select config) =
    Select { config | disabled = a }


{-| Set `caret` on a `Select`.

Identical to `Dropdown.caret`

-}
caret : Bool -> Select msg option -> Select msg option
caret a (Select config) =
    Select { config | caret = a }


{-| Set `fluid on a`Select\`.

Identical to `Dropdown.fluid`

-}
fluid : Bool -> Select msg option -> Select msg option
fluid a (Select config) =
    Select { config | fluid = a }


{-| Set `scrolling` on a `Select`.

Identical to `Dropdown.scrolling`

-}
scrolling : Bool -> Select msg option -> Select msg option
scrolling a (Select config) =
    Select { config | scrolling = a }


{-| Everything needed to build the `Html msg` representation of a particular select dropdown.

  - toDropdown - converts a Html element into a SemanticUI dropdown. Often used with `div` or `a`.
  - toOption - converts a Html element into a selectable menu item for the given option value.
  - drawer - the drawer element that can be toggled open or closed

-}
type alias Builder msg option =
    { toDropdown : Html.Builder msg -> Html.Builder msg
    , toOption : option -> Html.Builder msg -> Html.Builder msg
    , drawer : Html.Builder msg
    }


{-| A dropdown component with selectable options.

This is a vanilla Select component that can be extended in many different ways.
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
        , caret = dropdownConfig.caret
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
        dropdownLayout { toDropdown, drawer } =
            layout
                { toDropdown = toDropdown
                , drawer =
                    drawer
                        |> Html.prependAttribute (onClick (config.onToggle Drawer.Closing))
                , toOption =
                    \value ->
                        toItem << Html.prependAttributes (onClick (config.onSelect value) :: config.optionAttributes value)
                }
    in
    Dropdown
        { variation =
            case config.variation of
                Button but ->
                    Dropdown.Button but

                _ ->
                    Dropdown.Ordinary
        , uiClassList = variationClassList config.variation
        , drawerState = config.drawerState
        , identifier = config.identifier
        , onToggle = config.onToggle
        , attributes = config.attributes
        , toggleEvent = config.toggleEvent
        , disabled = config.disabled
        , caret = config.caret
        , labels = config.labels
        , fluid = config.fluid
        , scrolling = config.scrolling
        }
        |> Dropdown.toCustomHtml dropdownLayout


{-| Converts a HTML element into a SemanticUI menu item. Often used with `div` or `a`.

Identical to `Dropdown.toItem`
-}
toItem : Html.Builder msg -> Html.Builder msg
toItem =
    Dropdown.toItem


{-| Create a menu item that goes into the dropdown drawer, styled as if it were an `a` element.

Identical to `Dropdown.linkItem`
-}
linkItem : List (Attribute msg) -> List (Html msg) -> Html msg
linkItem =
    Dropdown.linkItem
