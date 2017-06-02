module SemanticUI.Elements.Button exposing (button, link, viewAs, Model, Emphasis(..), emphasis, primary, secondary)

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

@docs Model

## Emphasis

@docs primary, secondary, Emphasis, emphasis

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import SemanticUI exposing (..)


{-| Enumeration of possible types of emphasis
-}
type Emphasis
    = Primary
    | Secondary


type Animation
    = DefaultAnimation
    | VerticalAnimation
    | FadeAnimation


type alias HiddenContent msg =
    { hiddenContent : List (Html msg)
    , animation : Animation
    }


{-| All properties of a button
-}
type alias Model msg =
    { emphasis : Maybe Emphasis
    , hiddenContent : Maybe (HiddenContent msg)
    , basic : Bool
    , inverted : Bool
    , loading : Bool
    , fluid : Bool
    , attributes : List (Attribute msg)
    , size : Size
    }


fluid : Bool -> Model msg -> Model msg
fluid fluid model =
    { model | fluid = fluid }


size : Size -> Model msg -> Model msg
size size model =
    { model | size = size }


attributes : List (Attribute msg) -> Model msg -> Model msg
attributes attrs model =
    { model | attributes = attrs }


init : Model msg
init =
    { emphasis = Nothing
    , hiddenContent = Nothing
    , inverted = False
    , basic = False
    , fluid = False
    , loading = False
    , attributes = []
    , size = Medium
    }


basic : Bool -> Model msg -> Model msg
basic basic model =
    { model | basic = basic }


loading : Bool -> Model msg -> Model msg
loading loading model =
    { model | loading = loading }


inverted : Bool -> Model msg -> Model msg
inverted inverted model =
    { model | inverted = inverted }


{-| Set (or clear) the emphasis on a button.
-}
emphasis : Maybe Emphasis -> Model msg -> Model msg
emphasis emphasis model =
    { model | emphasis = emphasis }


{-| Set the emphasis of a button to Primary.
-}
primary : Model msg -> Model msg
primary model =
    { model | emphasis = Just Primary }


{-| Set the emphasis of a button to Secondary.
-}
secondary : Model msg -> Model msg
secondary model =
    { model | emphasis = Just Secondary }


hiddenContent :
    Maybe { animation : Animation, hiddenContent : List (Html msg) }
    -> Model msg
    -> Model msg
hiddenContent hiddenContent model =
    { model | hiddenContent = hiddenContent }


{-| View as a `<button>` element
-}
button : Model msg -> List (Html msg) -> Html msg
button =
    viewAs Html.button


{-| View as a `<a>` element
-}
link : Model msg -> List (Html msg) -> Html msg
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
    -> Model msg
    -> List (Html msg)
    -> Html msg
viewAs element { emphasis, hiddenContent, basic, inverted, loading, fluid, attributes, size } label =
    element
        (List.concat
            [ attributes
            , [ class "ui button"
              , classList
                    [ ( "primary", emphasis == Just Primary )
                    , ( "secondary", emphasis == Just Secondary )
                    , ( "basic", basic )
                    , ( "inverted", inverted )
                    , ( "loading", loading )
                    , ( "fluid", fluid )
                    , ( "inverted", inverted )
                    ]
              , sizeClass size
              ]
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
            ]
        )
    <|
        case hiddenContent of
            Nothing ->
                label

            Just { hiddenContent } ->
                [ div [ class "visible content" ] label
                , div [ class "hidden content" ] hiddenContent
                ]
