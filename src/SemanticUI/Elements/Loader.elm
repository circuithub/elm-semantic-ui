module SemanticUI.Elements.Loader exposing
    ( inline, inlineCentered
    , init, Config
    , active
    , size, text
    )

{-| A loader alerts a user to wait for an activity to complete.


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
import SemanticUI


{-| The configuration of a loader.
-}
type alias Config =
    { active : Bool
    , text : Maybe String
    , size : SemanticUI.Size
    }


{-| Basic configuration of a `Loader`. Corresponds to `class="ui active loader"`.

Note that this loader is active by default. If you don't want this, remember to
extend this configuration with `Loader.init |> Loader.active False`.

-}
init : Config
init =
    { active = True
    , text = Nothing
    , size = SemanticUI.Medium
    }


{-| Specify whether or not a loader is active.
-}
active : Bool -> Config -> Config
active a model =
    { model | active = a }


{-| Specify whether or not a loader is active.
-}
size : SemanticUI.Size -> Config -> Config
size a model =
    { model | size = a }


{-| View a loader inline with other content.
-}
inline : Config -> Html msg
inline cfg =
    view
        { active = cfg.active
        , text = cfg.text
        , inline = True
        , centered = False
        , size = cfg.size
        }


text : Maybe String -> Config -> Config
text t model =
    { model | text = t }


{-| View a loader inline centered with content.
-}
inlineCentered : Config -> Html msg
inlineCentered cfg =
    view
        { active = cfg.active
        , text = cfg.text
        , inline = True
        , centered = True
        , size = cfg.size
        }


view cfg =
    div
        [ class "ui loader"
        , classList
            [ ( "active", cfg.active )
            , ( "inline", cfg.inline )
            , ( "centered", cfg.centered )
            , ( "text", cfg.text /= Nothing )
            ]
        , SemanticUI.sizeClass cfg.size
        ]
        (case cfg.text of
            Nothing ->
                []

            Just t ->
                [ Html.text t ]
        )
