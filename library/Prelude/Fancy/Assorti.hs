module Prelude.Fancy.Assorti where

import Data.Map.Lazy (Map)

bind ∷ Monad monad ⇒ (input → monad output) → monad input → monad output
bind = (=<<)

type key ⇸ value = Map key value
