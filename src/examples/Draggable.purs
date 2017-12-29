module Examples.Draggable where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Array as Array
import Data.Maybe as Maybe
import React (ReactClass, ReactElement)
import React as R
import React.DOM as R
import React.DOM.Props as P

import Html (Html, div, text, ul, li)
import Html.Attributes (classes)
import Html as Html

foreign import dragDropContext :: forall eff. (Int -> Int -> Unit) -> ReactElement -> ReactElement
foreign import droppable :: forall props a b. props -> ReactElement -> ReactElement
foreign import draggable :: forall props a b. props -> ReactElement -> ReactElement


data Msg
  = Reorder Int Int

type State =
  { items :: Array String }


initialState :: State
initialState =
  { items: [ "Purescript", "Elm", "Typescript", "Bucklescript"]}

update :: Msg -> State -> { state :: State, effects :: Array Unit }
update msg state =
  case msg of
    Reorder source dest ->
      let
        reorder = do
          removed <- Array.index state.items source
          result  <- Array.deleteAt source state.items
          Array.insertAt dest removed result
      in
      Maybe.maybe state (\ordered -> state { items = ordered }) reorder
      # Html.noFx



itemView :: String -> Html Msg
itemView s =
  li [classes "drag-item"] [text s]


singleItem :: Html.Dispatch Msg -> String -> ReactElement
singleItem dispatch s =
  draggable { key: s, draggableId: s } (Html.unwrapHtml dispatch (itemView s))


buildChildren :: Html.Dispatch Msg -> Array String -> ReactElement
buildChildren dispatch items =
  R.ul [] $ map (singleItem dispatch) items


dragContainer :: Array String -> Html Msg
dragContainer items =
  Html.Html \dispatch ->
    let
      tagger a b =
        dispatch (Reorder a b)
      
      children =
        buildChildren dispatch items
    in
    droppable { droppableId: "droppable" } children
    # dragDropContext tagger 



view :: State -> Html Msg
view state =
  div [] [
    dragContainer state.items
  ]


program :: Eff _ (Html.Program Msg State _)
program = Html.program
  { view
  , update
  , initialState
  , subscriptions: []
  , effectManager: const (pure [])
  }
