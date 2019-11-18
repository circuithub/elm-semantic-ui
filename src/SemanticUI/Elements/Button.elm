module SemanticUI.Elements.Button exposing
    ( button, link, viewAs
    , Config, init, attributes
    , attached
    , primary, secondary, Emphasis(..), emphasis
    , hiddenContent, HiddenContent, Animation
    , fluid
    , size
    , basic
    , loading
    , inverted
    , floated
    , compact
    , icon, iconSide, IconSide(..)
    , active
    , disabled
    )

{-| Provides the [button](https://semantic-ui.com/elements/button.html) element
from Semantic UI.

Usage example:

    import SemanticUI.Elements.Button as Button
    import SemanticUI.Elements.Icon as Icon

    -- In your view function:
    ...
      Button.button
        (Button.init
          |> Button.primary
          |> Button.icon (Just Icon.Search))
        [ text "Search" ]


# Viewing buttons

@docs button, link, viewAs


# Button properties

@docs Config, init, attributes


## Attached

A button can be attached to the top or bottom of other content.

@docs attached


## Emphasis

A button can be formatted to show different levels of emphasis.

@docs primary, secondary, Emphasis, emphasis


## Animated buttons and hidden content

A button can animate to show hidden content.

@docs hiddenContent, HiddenContent, Animation


## Fluid

A button can take the width of its container.

@docs fluid


## Size

A button can have different sizes.

@docs size


## Basic

A basic button is less pronounced

@docs basic


## Loading

A button can show a loading indicator

@docs loading


## Inverted

A button can be formatted to appear on dark backgrounds.

@docs inverted


## Floated

A button can be aligned to the left or right of its container.

@docs floated


## Compact

A button can reduce its padding to fit into tighter spaces

@docs compact


## Icons

Buttons can have icons associated with them. If a button has icon and no content,
it will be formatted as an icon button. If a button has icon and content, it
will be displayed as a labelled button.

@docs icon, iconSide, IconSide


## Active

A button can show it is currently the active user selection.

@docs active


## Disabled

A button can show it is currently unable to be interacted with

@docs disabled

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import SemanticUI exposing (..)
import SemanticUI.Elements.Icon as Icon


{-| Enumeration of possible types of emphasis
-}
type Emphasis
    = Primary
    | Secondary
    | Negative
    | Positive


{-| How hidden content should be revealed.
-}
type Animation
    = DefaultAnimation
    | VerticalAnimation
    | FadeAnimation


{-| Hidden content along with its `Animation`
-}
type alias HiddenContent msg =
    { hiddenContent : List (Html msg)
    , animation : Animation
    }


{-| All properties of a button.
-}
type alias Config msg =
    { emphasis : Maybe Emphasis
    , hiddenContent : Maybe (HiddenContent msg)
    , basic : Bool
    , inverted : Bool
    , loading : Bool
    , fluid : Bool
    , attributes : List (Attribute msg)
    , size : Size
    , iconSide : IconSide
    , icon : Maybe Icon.Icon
    , floated : Maybe Floated
    , compact : Bool
    , active : Bool
    , attached : Maybe Attached
    , disabled : Bool
    }


{-| Whether or not this button is attached to the top or bottom of other content
-}
attached : Maybe Attached -> Config msg -> Config msg
attached a model =
    { model | attached = a }


{-| Whether or not this button is disabled
-}
disabled : Bool -> Config msg -> Config msg
disabled a model =
    { model | disabled = a }


{-| Whether or not this button is fluid
-}
fluid : Bool -> Config msg -> Config msg
fluid a model =
    { model | fluid = a }


{-| Whether or not this button is the current active user selection
-}
active : Bool -> Config msg -> Config msg
active a model =
    { model | active = a }


{-| The size of a button.
-}
size : Size -> Config msg -> Config msg
size a model =
    { model | size = a }


{-| Any other custom `Attribute`s to add to this button. Custom attributes
will be added before `elm-semantic-ui` attributes.
-}
attributes : List (Attribute msg) -> Config msg -> Config msg
attributes a model =
    { model | attributes = a }


{-| The initial config of a button. Corresponds to just using `class="ui button"` in Semantic UI.
-}
init : Config msg
init =
    { emphasis = Nothing
    , hiddenContent = Nothing
    , inverted = False
    , basic = False
    , fluid = False
    , loading = False
    , attributes = []
    , size = Medium
    , iconSide = IconLeft
    , floated = Nothing
    , compact = False
    , icon = Nothing
    , active = False
    , attached = Nothing
    , disabled = False
    }


{-| Whether or not a button is basic.
-}
basic : Bool -> Config msg -> Config msg
basic a model =
    { model | basic = a }


{-| Whether or not this button should display a loading spinner.
-}
loading : Bool -> Config msg -> Config msg
loading a model =
    { model | loading = a }


{-| Whether or not this button should display with inverted colours.
-}
inverted : Bool -> Config msg -> Config msg
inverted a model =
    { model | inverted = a }


{-| Set (or clear) the emphasis on a button.
-}
emphasis : Maybe Emphasis -> Config msg -> Config msg
emphasis a model =
    { model | emphasis = a }


{-| Set the emphasis of a button to Primary.
-}
primary : Config msg -> Config msg
primary model =
    { model | emphasis = Just Primary }


{-| Set the emphasis of a button to Secondary.
-}
secondary : Config msg -> Config msg
secondary model =
    { model | emphasis = Just Secondary }


{-| Add hidden content to a button.
-}
hiddenContent :
    Maybe (HiddenContent msg)
    -> Config msg
    -> Config msg
hiddenContent a model =
    { model | hiddenContent = a }


{-| View as a `<button>` element
-}
button : Config msg -> List (Html msg) -> Html msg
button =
    viewAs Html.button


{-| View as a `<a>` element
-}
link : Config msg -> List (Html msg) -> Html msg
link =
    viewAs a


{-| In Semantic UI, _any_ element can be a button. This generalised view
function allows you to supply an element and use it as a button. For example,

    Button.viewAs Html.input
        (Button.init |> Button.attributes [ Html.type_ "submit" ])
        [ text "Submit" ]

-}
viewAs :
    (List (Attribute msg) -> List (Html msg) -> Html msg)
    -> Config msg
    -> List (Html msg)
    -> Html msg
viewAs element cfg label =
    let
        contentWithIcon =
            List.concat
                [ case cfg.icon of
                    Just i ->
                        [ Icon.icon Icon.init i ]

                    Nothing ->
                        []
                , label
                ]

        labelled =
            cfg.icon /= Nothing && not (List.isEmpty label)
    in
    element
        (List.concat
            [ cfg.attributes
            , [ class "ui button"
              , classList
                    [ ( "basic", cfg.basic )
                    , ( "inverted", cfg.inverted )
                    , ( "loading", cfg.loading )
                    , ( "fluid", cfg.fluid )
                    , ( "inverted", cfg.inverted )
                    , ( "right", labelled && cfg.iconSide == IconRight )
                    , ( "labeled", labelled )
                    , ( "icon", cfg.icon /= Nothing )
                    , ( "active", cfg.active )
                    , ( "disabled", cfg.disabled )
                    , ( "compact", cfg.compact )
                    ]
              , sizeClass cfg.size
              ]
            , case cfg.attached of
                Nothing ->
                    []

                Just a ->
                    [ attachedClass a ]
            , case cfg.emphasis of
                Nothing ->
                    []

                Just Primary ->
                    [ class "primary" ]

                Just Secondary ->
                    [ class "secondary" ]

                Just Negative ->
                    [ class "negative" ]

                Just Positive ->
                    [ class "positive" ]
            , case cfg.floated of
                Nothing ->
                    []

                Just cls ->
                    [ floatedClass cls ]
            , case cfg.hiddenContent of
                Nothing ->
                    []

                Just { animation } ->
                    List.concat
                        [ [ class "animated" ]
                        , case animation of
                            DefaultAnimation ->
                                []

                            VerticalAnimation ->
                                [ class "vertical" ]

                            FadeAnimation ->
                                [ class "fade" ]
                        ]
            , [ Html.Attributes.disabled cfg.disabled ]
            ]
        )
    <|
        case cfg.hiddenContent of
            Nothing ->
                contentWithIcon

            Just a ->
                [ div [ class "visible content" ] contentWithIcon
                , div [ class "hidden content" ] a.hiddenContent
                ]


{-| Which side an icon should be displayed at.
-}
type IconSide
    = IconLeft
    | IconRight


{-| Specify which side of a button its icon should be displayed at.

This has no effect unless an icon has been associated with a button (see `icon`).

-}
iconSide : IconSide -> Config msg -> Config msg
iconSide a model =
    { model | iconSide = a }


{-| Specify whether on not a button should be floated.
-}
floated : Maybe Floated -> Config msg -> Config msg
floated a model =
    { model | floated = a }


{-| Whether or not this button is rendered compactly.
-}
compact : Bool -> Config msg -> Config msg
compact a model =
    { model | compact = a }


{-| Associate an icon with a button.
-}
icon : Maybe Icon.Icon -> Config msg -> Config msg
icon a model =
    { model | icon = a }
