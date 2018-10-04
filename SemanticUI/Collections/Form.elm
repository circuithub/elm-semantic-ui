module SemanticUI.Collections.Form
    exposing
        ( Config
        , attributes
        , error
        , form
        , init
        )

{-| A form displays a set of related user input fields in a structured way.


# Viewing forms

@docs form


# Form properties

@docs init, Config, attributes


## Errors

If a form is in an error state, it will automatically show any error message blocks.

@docs error

-}

import Html exposing (..)
import Html.Attributes exposing (..)


{-| The configuration of a form.
-}
type alias Config msg =
    { attributes : List (Attribute msg)
    , error : Bool
    }


{-| The simplest configuration of a form. Corresponds to `class="ui form"`.
-}
init : Config msg
init =
    { attributes = []
    , error = False
    }


{-| Any other custom `Attribute`s to add to this form. Custom attributes
will be added before `elm-semantic-ui` attributes.
-}
attributes : List (Attribute msg) -> Config msg -> Config msg
attributes attrs model =
    { model | attributes = attrs }


{-| Flag whether or not this form contains errors.
-}
error : Bool -> Config msg -> Config msg
error e model =
    { model | error = e }


{-| View a form as a `<form>` tag.
-}
form : Config msg -> List (Html msg) -> Html msg
form cfg contents =
    Html.form
        (List.concat
            [ cfg.attributes
            , [ class "ui form"
              , classList [ ( "error", cfg.error ) ]
              ]
            ]
        )
        contents
