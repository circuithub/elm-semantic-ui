module SemanticUI.Elements.Loader
    exposing
        ( Config
        , init
        , active
        , inline
        , inlineCentered
        , text
        )

{-|
A loader alerts a user to wait for an activity to complete.

# Viewing loaders

@docs inline, inlineCentered

# Loader properties

@docs init, Config

## Active

Loaders are hidden unless active or inside an active dimmer.

@docs active

-}

import Html exposing (..)
import Html.Attributes exposing (..)


{-| The configuration of a loader.
-}
type alias Config =
    { active : Bool
    , text : Maybe String
    }


{-| Basic configuration of a `Loader`. Corresponds to `class="ui active loader"`.

Note that this loader is active by default. If you don't want this, remember to
extend this configuration with `Loader.init |> Loader.active False`.

-}
init : Config
init =
    { active = True
    , text = Nothing
    }


{-| Specify whether or not a loader is active.
-}
active : Bool -> Config -> Config
active active model =
    { model | active = active }


{-| View a loader inline with other content.
-}
inline : Config -> Html msg
inline { active, text } =
    view { active = active, inline = True, centered = False, text = text }


text : Maybe String -> Config -> Config
text t model =
    { model | text = t }


{-| View a loader inline centered with content.
-}
inlineCentered : Config -> Html msg
inlineCentered { active, text } =
    view { active = active, inline = True, centered = True, text = text }


view { active, inline, centered, text } =
    div
        [ class "ui loader"
        , classList
            [ ( "active", active )
            , ( "inline", inline )
            , ( "centered", centered )
            , ( "text", text /= Nothing )
            ]
        ]
        (case text of
            Nothing ->
                []

            Just t ->
                [ Html.text t ]
        )
