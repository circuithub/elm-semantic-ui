module SemanticUI.Elements.Step exposing
    ( Config, steps
    , StepCount(..)
    , step, StepConfig, Step
    , link
    )

{-| A step shows the completion status of an activity in a series of activities.


# Viewing steps

@docs Config, steps


## Evenly divided steps

@docs StepCount


# Steps

@docs step, StepConfig, Step

@docs link

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
steps cfg theSteps =
    div
        (List.concat
            [ [ class "ui steps"
              , classList
                    [ ( "ordered", cfg.ordered )
                    , ( "vertical", cfg.vertical )
                    ]
              ]
            , case cfg.attached of
                Nothing ->
                    []

                Just a ->
                    [ attachedClass a ]
            , case cfg.stepCount of
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
        (List.map (\(Step a) -> a) theSteps)



-- Step


{-| The configuration of a single step.
-}
type alias StepConfig msg =
    { icon : Maybe Icon.Icon
    , title : Maybe String
    , completed : Bool
    , active : Bool
    , attributes : List (Attribute msg)
    }


{-| A single step in a list of steps.
-}
type Step msg
    = Step (Html msg)


{-| Construct a single step with a particular configuration.
-}
step : StepConfig msg -> List (Html msg) -> Step msg
step stepconfig content =
    viewAs div stepconfig content


{-| A step can link to another page.
-}
link : StepConfig msg -> List (Html msg) -> Step msg
link stepconfig content =
    viewAs a stepconfig content


viewAs el { icon, title, completed, active, attributes } content =
    Step <|
        el
            ([ class "step"
             , classList [ ( "completed", completed ), ( "active", active ) ]
             ]
                ++ attributes
            )
            (List.concat
                [ case icon of
                    Nothing ->
                        []

                    Just i ->
                        [ Icon.icon Icon.init i ]
                , [ div [ class "content" ] <|
                        case title of
                            Nothing ->
                                content

                            Just t ->
                                [ div [ class "title" ] [ text t ]
                                , div [ class "description" ] content
                                ]
                  ]
                ]
            )
