module SemanticUI.Elements.Button
    exposing
        ( button
        , link
        , viewAs
        , Config
        , Emphasis(..)
        , emphasis
        , primary
        , secondary
        , hiddenContent
        , HiddenContent
        , Animation
        , fluid
        , size
        , attributes
        , init
        , basic
        , loading
        , inverted
        , IconSide(..)
        , iconSide
        , floated
        , icon
        , active
        , attached
        , disabled
        )

{-|

Provides the [button](https://semantic-ui.com/elements/button.html) element
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
    , active : Bool
    , attached : Maybe Attached
    , disabled : Bool
    }


{-| Whether or not this button is attached to the top or bottom of other content
-}
attached : Maybe Attached -> Config msg -> Config msg
attached attached model =
    { model | attached = attached }


{-| Whether or not this button is disabled
-}
disabled : Bool -> Config msg -> Config msg
disabled disabled model =
    { model | disabled = disabled }


{-| Whether or not this button is fluid
-}
fluid : Bool -> Config msg -> Config msg
fluid fluid model =
    { model | fluid = fluid }


{-| Whether or not this button is the current active user selection
-}
active : Bool -> Config msg -> Config msg
active active model =
    { model | active = active }


{-| The size of a button.
-}
size : Size -> Config msg -> Config msg
size size model =
    { model | size = size }


{-| Any other custom `Attribute`s to add to this button. Custom attributes
 will be added before `elm-semantic-ui` attributes.
-}
attributes : List (Attribute msg) -> Config msg -> Config msg
attributes attrs model =
    { model | attributes = attrs }


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
    , icon = Nothing
    , active = False
    , attached = Nothing
    , disabled = False
    }


{-| Whether or not a button is basic.
-}
basic : Bool -> Config msg -> Config msg
basic basic model =
    { model | basic = basic }


{-| Whether or not this button should display a loading spinner.
-}
loading : Bool -> Config msg -> Config msg
loading loading model =
    { model | loading = loading }


{-| Whether or not this button should display with inverted colours.
-}
inverted : Bool -> Config msg -> Config msg
inverted inverted model =
    { model | inverted = inverted }


{-| Set (or clear) the emphasis on a button.
-}
emphasis : Maybe Emphasis -> Config msg -> Config msg
emphasis emphasis model =
    { model | emphasis = emphasis }


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
hiddenContent hiddenContent model =
    { model | hiddenContent = hiddenContent }


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


{-| In Semantic UI, *any* element can be a button. This generalised view
 function allows you to supply an element and use it as a button. For example,

    Button.viewAs Html.input
      (Button.init |> Button.attributes [ Html.type_ "submit"])
      [ text "Submit" ]
-}
viewAs :
    (List (Attribute msg) -> List (Html msg) -> Html msg)
    -> Config msg
    -> List (Html msg)
    -> Html msg
viewAs element { emphasis, hiddenContent, basic, inverted, loading, fluid, attributes, size, icon, iconSide, floated, active, attached, disabled } label =
    let
        contentWithIcon =
            List.concat
                [ case icon of
                    Just icon ->
                        [ Icon.icon Icon.init icon ]

                    Nothing ->
                        []
                , label
                ]

        labelled =
            icon /= Nothing && not (List.isEmpty label)
    in
        element
            (List.concat
                [ attributes
                , [ class "ui button"
                  , classList
                        [ ( "basic", basic )
                        , ( "inverted", inverted )
                        , ( "loading", loading )
                        , ( "fluid", fluid )
                        , ( "inverted", inverted )
                        , ( "right", labelled && iconSide == IconRight )
                        , ( "labeled", labelled )
                        , ( "icon", icon /= Nothing )
                        , ( "active", active )
                        , ( "disabled", disabled )
                        ]
                  , sizeClass size
                  ]
                , case attached of
                    Nothing ->
                        []

                    Just attached ->
                        [ attachedClass attached ]
                , case emphasis of
                    Nothing ->
                        []

                    Just emphasis ->
                        case emphasis of
                            Primary ->
                                [ class "primary" ]

                            Secondary ->
                                [ class "secondary" ]

                            Negative ->
                                [ class "negative" ]
                , case floated of
                    Nothing ->
                        []

                    Just floated ->
                        [ floatedClass floated ]
                , case hiddenContent of
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
                , [ Html.Attributes.disabled disabled ]
                ]
            )
        <|
            case hiddenContent of
                Nothing ->
                    contentWithIcon

                Just { hiddenContent } ->
                    [ div [ class "visible content" ] contentWithIcon
                    , div [ class "hidden content" ] hiddenContent
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
iconSide iconSide model =
    { model | iconSide = iconSide }


{-| Specify whether on not a button should be floated.
-}
floated : Maybe Floated -> Config msg -> Config msg
floated floated model =
    { model | floated = floated }


{-| Associate an icon with a button.
-}
icon : Maybe Icon.Icon -> Config msg -> Config msg
icon icon model =
    { model | icon = icon }
