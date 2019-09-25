module SemanticUI.Modules.Dropdown.Select exposing
    ( Builder
    , Config
    , DrawerState(..)
    , Select(..)
    , ToggleEvent(..)
    , readOnly
    , select
    , toHtml
    , toggleEvent
    )

{-| Refine a dropdown with selection state

Example of `Select.select` :

    let
        menu { toSelect, toOption, toToggle, drawer } =
            let
                option val =
                    toOption val div [ class "text" ] [ text (toString val) ]
            in
            toSelect div
                [ class "inline" ]
                [ toToggle div
                    [ classList [ ( "default", model.select1Selection == Nothing ), ( "text", True ) ] ]
                    [ text (Maybe.withDefault "Nothing selected" model.select1Selection)
                    , i [ class "dropdown icon" ] []
                    ]
                , drawer
                    []
                    (List.map option [ Yes, No ])
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


{-| Configuration for a select

  - identifier - unique identifier
  - onToggle - message to handle dropdown state changes
  - toggleEvent - what casuse dropdown state change (default OnClick)
  - readOnly - is the selection read only or not
  - onSelect - messages to handle selection change (when clicking on option)
  - layout
  - layoutOption

-}
type alias Config msg option =
    { drawerState : DrawerState
    , identifier : String
    , onSelect : option -> msg
    , onToggle : DrawerState -> msg
    , toggleEvent : ToggleEvent
    , readOnly : Bool
    , selectAttributes : List (Attribute msg)
    , optionAttributes : option -> List (Attribute msg)
    , selectLabels : List (Html msg)
    , formInput : Maybe { name : String, value : String }
    }


type Select msg option
    = Select (Config msg option)


{-| Set `toggleEvent` on a `Select`
-}
toggleEvent : ToggleEvent -> Select msg option -> Select msg option
toggleEvent a (Select config) =
    Select { config | toggleEvent = a }


{-| Set `readOnly` on a `Select`
-}
readOnly : Bool -> Select msg option -> Select msg option
readOnly a (Select config) =
    Select { config | readOnly = a }


{-| Everything needed to build the `Html msg` representation of a particular select dropdown.
-}
type alias Builder msg option =
    { toSelect : HtmlBuilder msg -> HtmlBuilder msg
    , toToggle : HtmlBuilder msg -> HtmlBuilder msg
    , toOption : option -> HtmlBuilder msg -> HtmlBuilder msg
    , drawer : HtmlBuilder msg
    }


{-| A dropdown component with stateful selection.

Serves as the basis for several other select component, including multiple selection.
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
        { drawerState = config.drawerState
        , identifier = config.identifier
        , onToggle = config.onToggle
        , onSelect = config.onSelect
        , toggleEvent = OnClick
        , readOnly = False
        , selectAttributes = []
        , optionAttributes = \_ -> []
        , selectLabels = []
        , formInput = Nothing
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
        , defaultLabel : String
        , selectionLabel : option -> String
    }
    -> Select msg option
labeled config =
    Select
        { drawerState = config.drawerState
        , identifier = config.identifier
        , onToggle = config.onToggle
        , onSelect = config.onSelect
        , formInput = Nothing
        , selectAttributes = [ class "labeled" ]
        , optionAttributes =
            \option ->
                [ classList [ ( "active selected", Just option == config.currentSelection ) ] ]
        , selectLabels =
            [ case config.currentSelection of
                Nothing ->
                    span [ class "default text" ] [ text config.defaultLabel ]

                Just selectedOption ->
                    span [ class "text" ] [ text (config.selectionLabel selectedOption) ]
            ]
        , toggleEvent = OnClick
        , readOnly = False
        }


{-| A dropdown select component visually styled as a `<select>` form control.

This configuration can e used with an optional hidden `<input>` for forms.

-}
selection :
    { config
        | currentSelection : Maybe option
        , drawerState : DrawerState
        , identifier : String
        , onToggle : DrawerState -> msg
        , onSelect : option -> msg
        , formInput : Maybe { name : String, toValue : option -> String }
        , defaultLabel : String
        , selectionLabel : option -> String
    }
    -> Select msg option
selection config =
    let
        (Select labeledConfig) =
            labeled config
    in
    Select
        { labeledConfig
            | selectAttributes = [ class "selection" ]
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
        , defaultLabel : String
        , selectionLabel : option -> String
    }
    -> Select msg option
inline config =
    let
        (Select labeledConfig) =
            labeled config
    in
    Select
        { labeledConfig
            | selectAttributes = [ class "inline" ]
        }


toHtml : (Builder msg option -> Html msg) -> Select msg option -> Html msg
toHtml layout (Select config) =
    let
        dropdownLayout { toDropdown, toToggle, drawer } =
            layout
                { toSelect =
                    \element ->
                        toDropdown element
                            |> HtmlBuilder.prependChildren
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
        , toggleEvent = toDropdownToggleEvent config.toggleEvent
        , readOnly = config.readOnly
        }
        |> Dropdown.toHtml dropdownLayout


{-| Create an item that goes in the drawer.

Identical to `Dropdown.toItem`

-}
toItem : HtmlBuilder msg -> HtmlBuilder msg
toItem =
    HtmlBuilder.prependAttribute (class "item")


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
