module Html
  ( module Exports
  , Html(..)
  , Program
  , Subscriber
  , Dispatch
  , EffectManager
  , UpdateResult
  , unwrapHtml
  , createComponent
  , noFx, withFx
  , program
  , text, input, textarea, br, a, ul, li, div, button, h1, table, tbody, tr, td, span
  , nav, i, b, em, strong, header, footer, img, fieldset, p, article, section, main_
  )
  where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff as Aff
import Control.Monad.Eff.Unsafe (unsafePerformEff) as Unsafe
import Control.Monad.Eff.Class (liftEff)
import Control.Parallel as Parallel
import Data.Array as Array
import Html.Attributes (Attribute, unwrapAttr)
import Html.Attributes (Attribute) as Exports
import React (ReactClass)
import React as React
import React.DOM as DOM
import React.DOM.Props as P
import Signal as Signal
import Signal.Channel as Channel


-- This is clearly wrong, it should be `msg -> Eff eff Unit`
-- But that would require `Html msg` to be updated as well
-- into `Html msg eff`, which I would rather avoid.
-- In short, let's wait for `IO` :)
type Dispatch msg =
  msg -> Unit

newtype Html msg =
  Html (Dispatch msg -> React.ReactElement)

instance functorHtml :: Functor Html where
  map tagger (Html html) =
    Html \dispatch -> html (dispatch <<< tagger)

type Element msg =
  Array (Attribute msg) -> Array (Html msg) -> Html msg

type Subscriber state eff =
  (state -> Eff eff Unit) -> Eff eff Unit

type Program msg state fx eff =
  { render       :: ReactClass state
  , subscribe    :: Subscriber state eff
  , init         :: UpdateResult state fx
  }

type Update msg state fx =
  msg -> state -> (UpdateResult state fx)

type UpdateResult state fx =
  { state :: state, effects :: Array fx }

type EffectManager msg fx eff =
  fx -> Aff eff (Array msg)

type App msg state fx eff =
  { view          :: state -> Html msg
  , update        :: Update msg state fx
  , init          :: UpdateResult state fx
  , subscriptions :: Array (Signal.Signal (Array msg))
  , effectManager :: EffectManager msg fx eff
  }


unwrapHtml :: forall msg. Dispatch msg -> Html msg -> React.ReactElement
unwrapHtml dispatch (Html fn) =
  fn dispatch


elem :: forall msg
  . ((Array P.Props) -> (Array React.ReactElement) -> React.ReactElement)
  -> Array (Attribute msg)
  -> Array (Html msg)
  -> Html msg
elem node attrs children =
  Html \dispatch ->
    node
      (map (unwrapAttr dispatch) attrs)
      (map (unwrapHtml dispatch) children)


foldUpdate :: forall msg state fx
  .  Update msg state fx
  -> Array msg
  -> (UpdateResult state fx)
  -> (UpdateResult state fx)
foldUpdate update msgs result =
  let
    folder { state } msg =
      update msg state
  in
  Array.foldl folder result msgs


runEffect :: forall msg fx
  .  Channel.Channel (Array msg)
  -> EffectManager msg fx _
  -> fx
  -> Aff _ Unit
runEffect eventCh effectManager fx = do
  events <- effectManager fx
  case events of
    [] -> pure unit
    _  -> liftEff $ Channel.send eventCh events


runner :: forall msg state fx
  .  Channel.Channel (Array msg)
  -> (state -> Eff _ Unit)
  -> EffectManager msg fx _
  -> UpdateResult state fx
  -> Eff _ Unit
runner eventCh fn effectManager result =
  Aff.launchAff_ do
    liftEff (fn result.state)
    Parallel.parTraverse_ (runEffect eventCh effectManager) result.effects


subscribe :: forall msg state fx
  .  App msg state fx _
  -> Channel.Channel (Array msg)
  -> Subscriber state _
subscribe app eventCh fn =
  let
    events =
      Array.foldl Signal.merge (Channel.subscribe eventCh) app.subscriptions

    signal =
      Signal.foldp (foldUpdate app.update) app.init events

    process =
      runner eventCh fn app.effectManager
  in
  Signal.runSignal (map process signal)


render :: forall msg state. Dispatch msg -> (state -> Html msg) -> React.ReactClass state
render dispatch view =
  React.createClassStateless \state ->
    unwrapHtml dispatch (view state)


program :: forall msg state fx. App msg state fx _ -> Eff _ (Program msg state fx _)
program app = do
  eventCh <- Channel.channel []

  let
    dispatch msg =
      Channel.send eventCh [msg]
      # Unsafe.unsafePerformEff -- see comment at the top of the file

  pure
    { render: render dispatch app.view
    , subscribe: subscribe app eventCh
    , init: app.init
    }


noFx :: forall state. state -> UpdateResult state _
noFx state =
  { state, effects: [] }


withFx :: forall state fx. Array fx -> state -> UpdateResult state fx
withFx effects state =
  { state, effects }


-- Creates a `ReactComponent` out of a `Program`.
-- Useful when you just want to run the app
-- and don't want to deal with all the wirings yourself.
-- Alternatively, use `render` and `subscribe` directly.
createComponent :: forall msg state fx. Program msg state fx _ -> React.ReactClass state
createComponent program =
  let
    el =
      React.createElement program.render

    render ctx = do
      state <- React.readState ctx
      pure $ el state []

    initialState =
      program.init.state
  in
  React.createClass $
    (React.spec initialState render)
      { componentDidMount = \ctx ->
        program.subscribe \state ->
          void $ React.writeState ctx state
      }


-- Elements

text :: forall msg. String -> Html msg
text str =
  elem (\_ _ -> DOM.text str) [] []

input :: forall msg. Element msg
input = elem DOM.input

textarea :: forall msg. Element msg
textarea = elem DOM.textarea

br :: forall msg. Element msg
br = elem DOM.br

a :: forall msg. Element msg
a = elem DOM.a

ul :: forall msg. Element msg
ul = elem DOM.ul

li :: forall msg. Element msg
li = elem DOM.li

div :: forall msg. Element msg
div = elem DOM.div

button :: forall msg. Element msg
button = elem DOM.button

h1 :: forall msg. Element msg
h1 = elem DOM.h1

table :: forall msg. Element msg
table = elem DOM.table

tbody :: forall msg. Element msg
tbody = elem DOM.tbody

tr :: forall msg. Element msg
tr = elem DOM.tr

td :: forall msg. Element msg
td = elem DOM.td

span :: forall msg. Element msg
span = elem DOM.span

nav :: forall msg. Element msg
nav = elem DOM.nav

i :: forall msg. Element msg
i = elem DOM.i

b :: forall msg. Element msg
b = elem DOM.b

em :: forall msg. Element msg
em = elem DOM.em

strong :: forall msg. Element msg
strong = elem DOM.strong

header :: forall msg. Element msg
header = elem DOM.header

footer :: forall msg. Element msg
footer = elem DOM.footer

img :: forall msg. Element msg
img = elem DOM.img

fieldset :: forall msg. Element msg
fieldset = elem DOM.fieldset

p :: forall msg. Element msg
p = elem DOM.p

article :: forall msg. Element msg
article = elem DOM.article

section :: forall msg. Element msg
section = elem DOM.section

main_ :: forall msg. Element msg
main_ = elem DOM.main
