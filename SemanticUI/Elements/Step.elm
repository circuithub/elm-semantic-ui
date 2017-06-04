module SemanticUI.Elements.Step
    exposing
        ( StepCount(..)
        , StepConfig
        , Step
        , Config
        , steps
        , step
        )

{-| A step shows the completion status of an activity in a series of activities.

# Viewing steps

@docs Config, steps

## Evenly divided steps

@docs StepCount

# Steps

@docs step, StepConfig, Step
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import SemanticUI exposing (..)
import SemanticUI.Elements.Icon as Icon


{-| A set of steps can specify exactly how many steps proceed.
-}
type StepCount
    = One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight


{-| The configuration of a steps container.
-}
type alias Config =
    { attached : Maybe Attached
    , stepCount : Maybe StepCount
    , ordered : Bool
    , vertical : Bool
    }


{-| Render a list of steps as HTML.
-}
steps : Config -> List (Step msg) -> Html msg
steps { attached, stepCount, ordered, vertical } steps =
    div
        (List.concat
            [ [ class "ui steps"
              , classList
                    [ ( "ordered", ordered )
                    , ( "vertical", vertical )
                    ]
              ]
            , case attached of
                Nothing ->
                    []

                Just attached ->
                    [ attachedClass attached ]
            , case stepCount of
                Nothing ->
                    []

                Just count ->
                    [ class <|
                        case count of
                            One ->
                                "one"

                            Two ->
                                "two"

                            Three ->
                                "three"

                            Four ->
                                "four"

                            Five ->
                                "five"

                            Six ->
                                "six"

                            Seven ->
                                "seven"

                            Eight ->
                                "eight"
                    ]
            ]
        )
        (List.map (\(Step a) -> a) steps)



-- Step


{-| The configuration of a single step.
-}
type alias StepConfig =
    { icon : Maybe Icon.Icon
    , title : Maybe String
    , completed : Bool
    }


{-| A single step in a list of steps.
-}
type Step msg
    = Step (Html msg)


{-| Construct a single step with a particular configuration.
-}
step : StepConfig -> List (Html msg) -> Step msg
step { icon, title, completed } content =
    Step <|
        div
            [ class "step"
            , classList [ ( "completed", completed ) ]
            ]
            (List.concat
                [ case icon of
                    Nothing ->
                        []

                    Just icon ->
                        [ Icon.icon Icon.init icon ]
                , [ div [ class "content" ] <|
                        case title of
                            Nothing ->
                                content

                            Just title ->
                                [ div [ class "title" ] [ text title ]
                                , div [ class "description" ] content
                                ]
                  ]
                ]
            )
