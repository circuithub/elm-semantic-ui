module SemanticUI.Views.Cards.Card exposing (..)

{-| View a single card.


# Viewing cards

@docs linkCard


# Card properties

@docs init, Config, attributes


## Fluid

A fluid card takes up the width of its container.

@docs fluid

-}

import Html exposing (..)
import Html.Attributes exposing (..)


{-| The configuration of a card.
-}
type alias Config msg =
    { fluid : Bool
    , attributes : List (Attribute msg)
    }


{-| Specify whether or not a card is fluid.
-}
fluid : Bool -> Config msg -> Config msg
fluid a model =
    { model | fluid = a }


{-| Custom attributes for a card.
-}
attributes : List (Attribute msg) -> Config msg -> Config msg
attributes a model =
    { model | attributes = a }


{-| The simplest configuration of a card. Corresponds to `class="ui card"`.
-}
init : Config msg
init =
    { fluid = False
    , attributes = []
    }


{-| View a card as a link. Cards can contain a variety of content,
which can be supplied when viewing the card. An empty lists will be
omitted entirely.
-}
linkCard :
    Config msg
    ->
        { image : Maybe String
        , content : List (Html msg)
        , button : Maybe (Html msg)
        , extraContent : List (Html msg)
        }
    -> Html msg
linkCard =
    viewCard a


divCard :
    Config msg
    ->
        { image : Maybe String
        , content : List (Html msg)
        , button : Maybe (Html msg)
        , extraContent : List (Html msg)
        }
    -> Html msg
divCard =
    viewCard div


viewCard el cfg content =
    el
        (List.concat
            [ [ class "ui card"
              , classList [ ( "fluid", cfg.fluid ) ]
              ]
            , cfg.attributes
            ]
        )
        (List.concat
            [ case content.image of
                Nothing ->
                    []
                Just url ->
                    [ div [ class "image" ]
                          [ img [ src url ] [] ]
                    ]
            , case content.content of
                [] ->
                    []

                _ ->
                    [ div [ class "content" ] content.content ]
            , case content.button of
                Nothing ->
                    []

                Just button ->
                    [button]

            , case content.extraContent of
                [] ->
                    []

                _ ->
                    [ div [ class "extra content" ] content.extraContent ]
            ]
        )
