module Html.Attributes where

import Prelude

import Data.Array as Array
import Data.String as String
import Data.Tuple (Tuple(..))
import React.DOM.Props as P


newtype Attribute msg =
  Attribute ((msg -> Unit) -> P.Props)


unwrapAttr :: forall msg. (msg -> Unit) -> Attribute msg -> P.Props
unwrapAttr dispatch (Attribute prop) = prop dispatch


attr :: forall a msg. (a -> P.Props) -> a -> Attribute msg
attr prop val =
  Attribute (\_ -> prop val)


type_ :: forall msg. String -> Attribute msg
type_ = attr P.inputMode

value :: forall msg. String -> Attribute msg
value = attr P.value

href :: forall msg. String -> Attribute msg
href = attr P.href

attribute :: forall msg. String -> String -> Attribute msg
attribute key = attr (P.unsafeMkProps key)

id :: forall msg. String -> Attribute msg
id = attr P._id

classes :: forall msg. String -> Attribute msg
classes = attr P.className

selected :: forall msg. Boolean -> Attribute msg
selected = attr P.selected

key :: forall msg. String -> Attribute msg
key = attr P.key

src :: forall msg. String -> Attribute msg
src = attr P.src

alt :: forall msg. String -> Attribute msg
alt = attr P.alt

tabindex :: forall msg. Int -> Attribute msg
tabindex = attr P.tabIndex

classList :: forall msg. Array (Tuple String Boolean) -> Attribute msg
classList input =
  let
    folder acc (Tuple cl include) =
      if include
        then Array.cons cl acc
        else acc

    valid =
      Array.foldl folder [] input
  in
  classes (String.joinWith " " valid)

style :: forall msg. Array (Tuple String String) -> Attribute msg
style input =
  input
  # map (\(Tuple k v) -> k <> ": " <> v)
  # String.joinWith "; "
  # attribute "style"

placeholder :: forall msg. String -> Attribute msg
placeholder = attr P.placeholder
