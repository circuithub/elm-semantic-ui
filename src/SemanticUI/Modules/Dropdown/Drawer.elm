module SemanticUI.Modules.Dropdown.Drawer exposing (State(..), isToggled, isTransitioning, isVisible)

{-| The dropdown drawer

@docs State, isToggled, isTransitioning, isVisible

-}


{-| The current state of the dropdown drawer.
-}
type State
    = Closed
    | Opening
    | Opened
    | Closing


{-| TODO
-}
isToggled : State -> Bool
isToggled state =
    state == Opening || state == Opened


{-| TODO
-}
isTransitioning : State -> Bool
isTransitioning state =
    state == Opening || state == Closing


{-| TODO
-}
isVisible : State -> Bool
isVisible state =
    state /= Closed
