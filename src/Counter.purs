module Counter where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff, Milliseconds(..))
import Control.Monad.Aff as Aff
import Data.Array as Array
import Data.Maybe as Maybe
import Html (Html, div, text, button, br, li, input)
import Html as Html
import Html.Attributes (classes, type_, value)
import Html.Events (onClick, onChange)
import Signal (Signal)
import Signal.Time as Signal

data Msg
  = Increment
  | Decrement
  | UpdateField String
  | Tick


type State =
  { count :: Int
  , value :: String
  , ticks :: Int
  }


initialState :: State
initialState =
  { count: 0
  , value: ""
  , ticks: 0
  }


update :: Msg -> State -> { state :: State, effects :: Array Unit }
update msg state =
  case msg of
    Increment ->
      Html.noFx $ state { count = state.count + 1 }

    Decrement ->
      Html.noFx $ state { count = state.count - 1 }

    UpdateField s ->
      Html.noFx $ state { value = s }

    Tick ->
      Html.noFx $ state { ticks = state.ticks + 1 }


view :: State -> Html Msg
view state =
  div []
    [ text ("Counter " <> (show state.count))
    , div [] [
        button [ onClick Increment ] [ text "Increment" ]
      , button [ onClick Decrement ] [ text "Decrement" ]
    ]
    , br [] []
    , br [] []

    , text ("Echo text: " <> state.value)
    , br [] []

    , input [ type_ "text", onChange UpdateField, value state.value ] []
    , br [] []
    , br [] []

    , text ("Time elapsed: " <> (show state.ticks))
    ]


ticker :: Signal (Array Msg)
ticker =
  Signal.every Signal.second
  # map (\_ -> [Tick])


program :: Eff _ (Html.Program Msg State _)
program = Html.program
  { view
  , update
  , initialState
  , subscriptions: [ticker]
  , effectManager: const (pure [])
  }
