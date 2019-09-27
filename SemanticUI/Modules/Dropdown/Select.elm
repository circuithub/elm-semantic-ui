module SemanticUI.Modules.Dropdown.Select exposing
    ( Builder
    , Config
    , DrawerState(..)
    , Select(..)
    , ToggleEvent(..)
    , attributes
    , dropdownIcon
    , fluid
    , inline
    , labeled
    , readOnly
    , select
    , selection
    , toHtml
    , toSimpleHtml
    , toggleEvent
    )

{-| Refine a dropdown with selection state

Example of `Select.labeled`:

    Select.labeled
        { identifier = "select1"
        , onToggle = ToggleSelect1
        , onSelect = SetSelect1
        , drawerState = model.select1DrawerState
        , defaultLabel = text "Select an option"
        , selectionLabel = text << ToString
        , currentSelection = model.select1Selection
        }
        |> Select.toSimpleHtml
            { optionLabel = text << toString, options = [ Yes, No ] }

Example of `Select.select` :

    let
        menu { toSelect, toOption, toToggle, drawer } =
            let
                option val =
                    toOption val div [] [ text (toString val) ]
            in
            toSelect div
                [ class "inline" ]
                [ toToggle div
                    [ classList [ ( "default", model.select1Selection == Nothing ), ( "text", True ) ] ]
                    [ text (Maybe.withDefault "Nothing selected" model.select1Selection)
                    , i [ class "dropdown icon" ] []
                    ]
                , drawer [] (List.map option [ Yes, No ])
                ]
    in
    Select.select
        { identifier = "select1"
        , onToggle = ToggleSelect1
        , onSelect = SetSelect1
        , drawerState = model.select1DrawerState
        }
        |> Select.toHtml menu

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


type Variation
    = Ordinary
    | Labeled
    | Inline
    | Selection


{-| Most general configuration that applies any Select.

It is recommended that you use `selection`, `inline`, `labeled` or `select` to construct this record.

-}
type alias Config msg option =
    { variation : Variation
    , drawerState : DrawerState
    , identifier : String
    , onSelect : option -> msg
    , onToggle : DrawerState -> msg
    , attributes : List (Attribute msg)
    , toggleEvent : ToggleEvent
    , readOnly : Bool
    , optionAttributes : option -> List (Attribute msg)
    , selectLabels : List (Html msg)
    , formInput : Maybe { name : String, value : String }
    , dropdownIcon : Bool
    , fluid : Bool
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
toggleEvent : ToggleEvent -> Select msg option -> Select msg option
toggleEvent a (Select config) =
    Select { config | toggleEvent = a }


{-| Set `readOnly` on any `Select`
-}
readOnly : Bool -> Select msg option -> Select msg option
readOnly a (Select config) =
    Select { config | readOnly = a }


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


{-| Everything needed to build the `Html msg` representation of a particular select dropdown.

  - toSelect - converts a `<div>` or an `<a>` element into a SemanticUI dropdown
  - toToggle - converts an element into a toggle for the dropdown
  - toOption - takes the option value and converts a `<div>` or an `<a>` element into a selectable option item
  - drawer - the drawer element that can be toggled open or closed

-}
type alias Builder msg option =
    { toSelect : HtmlBuilder msg -> HtmlBuilder msg
    , toToggle : HtmlBuilder msg -> HtmlBuilder msg
    , toOption : option -> HtmlBuilder msg -> HtmlBuilder msg
    , drawer : HtmlBuilder msg
    }


{-| A dropdown component with stateful selection.

A vanilla select component, which can be extended in many different ways (including multiple selection).
It does not highlight the current selection automatically.

-}
select :
    { config
        | drawerState : DrawerState
        , identifier : String
        , onToggle : DrawerState -> msg
        , onSelect : option -> msg
    }
    -> Select msg option
select config =
    Select
        { variation = Ordinary
        , drawerState = config.drawerState
        , identifier = config.identifier
        , onToggle = config.onToggle
        , onSelect = config.onSelect
        , attributes = []
        , toggleEvent = OnClick
        , readOnly = False
        , optionAttributes = \_ -> []
        , selectLabels = []
        , formInput = Nothing
        , dropdownIcon = False
        , fluid = False
        }


{-| A dropdown select component, visually labeled with the current selection.
-}
labeled :
    { config
        | currentSelection : Maybe option
        , drawerState : DrawerState
        , identifier : String
        , onToggle : DrawerState -> msg
        , onSelect : option -> msg
        , defaultLabel : Html msg
        , selectionLabel : option -> Html msg
    }
    -> Select msg option
labeled config =
    Select
        { variation = Labeled
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
        , toggleEvent = OnClick
        , readOnly = False
        , dropdownIcon = False
        , fluid = False
        }


{-| A dropdown select component visually styled as a `<select>` form control.

This configuration can be used with an optional hidden `<input>` for forms.

-}
selection :
    { config
        | currentSelection : Maybe option
        , drawerState : DrawerState
        , identifier : String
        , onToggle : DrawerState -> msg
        , onSelect : option -> msg
        , formInput : Maybe { name : String, toValue : option -> String }
        , defaultLabel : Html msg
        , selectionLabel : option -> Html msg
    }
    -> Select msg option
selection config =
    let
        (Select labeledConfig) =
            labeled config
    in
    Select
        { labeledConfig
            | variation = Selection
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


{-| A dropdown select component that can be used inline with text.
-}
inline :
    { config
        | currentSelection : Maybe option
        , drawerState : DrawerState
        , identifier : String
        , onToggle : DrawerState -> msg
        , onSelect : option -> msg
        , defaultLabel : Html msg
        , selectionLabel : option -> Html msg
    }
    -> Select msg option
inline config =
    let
        (Select labeledConfig) =
            labeled config
    in
    Select
        { labeledConfig
            | variation = Inline
            , dropdownIcon = True
        }


toSimpleHtml : { builder | optionLabel : option -> Html msg, options : List option } -> Select msg option -> Html msg
toSimpleHtml { optionLabel, options } selectControl =
    let
        layout { toSelect, toOption, drawer } =
            let
                option val =
                    toOption val div [] [ optionLabel val ]
            in
            toSelect div [] [ drawer [] (List.map option options) ]
    in
    toHtml layout selectControl


toHtml : (Builder msg option -> Html msg) -> Select msg option -> Html msg
toHtml layout (Select config) =
    let
        dropdownLayout { toDropdown, toToggle, drawer } =
            layout
                { toSelect =
                    \element ->
                        toDropdown element
                            |> HtmlBuilder.appendAttributes
                                [ classList
                                    [ ( "inline", config.variation == Inline )
                                    , ( "labeled", config.variation == Labeled )
                                    , ( "selection", config.variation == Selection )
                                    ]
                                ]
                            |> HtmlBuilder.appendChildren
                                ((config.formInput
                                    |> Maybe.map (\formInput -> [ input [ name formInput.name, value formInput.value ] [] ])
                                    |> Maybe.withDefault []
                                 )
                                    ++ config.selectLabels
                                )
                , toToggle = toToggle
                , drawer =
                    drawer
                        |> HtmlBuilder.prependAttribute (onClick (config.onToggle Closing))
                , toOption =
                    \value ->
                        toItem << HtmlBuilder.prependAttributes (onClick (config.onSelect value) :: config.optionAttributes value)
                }
    in
    Dropdown.Dropdown
        { drawerState = toDropdownDrawerState config.drawerState
        , identifier = config.identifier
        , onToggle = config.onToggle << fromDropdownDrawerState
        , attributes = config.attributes
        , toggleEvent = toDropdownToggleEvent config.toggleEvent
        , readOnly = config.readOnly
        , dropdownIcon = config.dropdownIcon
        , fluid = config.fluid
        }
        |> Dropdown.toHtml dropdownLayout


{-| Create an item that goes in the drawer.

Identical to `Dropdown.toItem`

-}
toItem : HtmlBuilder msg -> HtmlBuilder msg
toItem =
    Dropdown.toItem


{-| Create a menu item that goes in the drawer, formatted as if it were an `<a>` element.

Identical to \`Dropdown.linkItem

-}
linkItem : List (Attribute msg) -> List (Html msg) -> Html msg
linkItem =
    Dropdown.linkItem


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
