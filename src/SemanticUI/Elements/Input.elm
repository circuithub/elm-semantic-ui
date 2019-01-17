module SemanticUI.Elements.Input exposing (..)

{-| An input is a field used to elicit a response from a user.

# Viewing inputs

@docs input

# Input properties

@docs Config, init

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import SemanticUI exposing (..)


{-| All properties of an input field.
-}
type alias Config msg =
    { attributes : List (Attribute msg)
    , focus : Bool
    , loading : Bool
    , error : Bool
    , transparent : Bool
    , inverted : Bool
    , fluid : Bool
    , size : Size
    }


{-| The initial config of an input field. Corresponds to just using `class="ui input"` in Semantic UI.
-}
init : Config msg
init =
    { attributes = []
    , focus = False
    , loading = False
    , error = False
    , transparent = False
    , inverted = False
    , fluid = False
    , size = Medium
    }


{-| View an `<input>` field with a particular configuration.
-}
input : Config msg -> Html msg
input { focus, attributes, loading, error, transparent, fluid, size, inverted } =
    Html.div
        [ class "ui input"
        , classList
            [ ( "focus", focus )
            , ( "loading", loading )
            , ( "error", error )
            , ( "transparent", transparent )
            , ( "inverted", inverted )
            , ( "fluid", fluid )
            ]
        , sizeClass size
        ]
        [ Html.input attributes [] ]
