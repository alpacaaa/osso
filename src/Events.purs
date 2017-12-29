module Html.Events where

import Prelude
import React.DOM.Props as P
import Unsafe.Coerce as Unsafe
import Html.Attributes (Attribute(..))


onClick :: forall msg. msg -> Attribute msg
onClick msg =
  Attribute \dispatch ->
    P.onClick \_ -> pure (dispatch msg)


onChange :: forall msg. (String -> msg) -> Attribute msg
onChange msg =
  Attribute \dispatch ->
    P.onChange \e ->
      let
        value = (Unsafe.unsafeCoerce e).target.value
      in
      pure (dispatch $ msg value)
