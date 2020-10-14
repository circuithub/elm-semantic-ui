module SemanticUI.Modules.Dropdown.Toggle exposing (Event(..))

{-| A shared event type for the various dropdown modules.

@docs Event

-}


{-| Event that triggers the dropdown drawer to an open state.
-}
type Event
    = OnClick
    | OnHover
    | OnFocus
