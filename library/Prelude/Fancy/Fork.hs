module Prelude.Fancy.Fork where

import Data.Kind
import Prelude.Fancy.Arithmetic
import Prelude.Fancy.Curry

type family Fan source (targets ∷ [★]) = result | result → targets where
  Fan source '[] = ()
  Fan source (target : targets) = (source → target) × Fan source targets

type Fork ∷ ★ → [★] → Constraint
class Fork source targets where fork_ ∷ Fan source targets → source → Tuple targets
instance Fork source '[] where fork_ () = \_ → ()
instance Fork source targets ⇒ Fork source (target : targets) where
  fork_ (arrow :× arrows) = \source → arrow source :× fork_ arrows source

type family ForkSource arrow where
  ForkSource ((source → target) → arrow) = ForkSource arrow
  ForkSource (source → target) = source

type family ForkTargets arrow where
  ForkTargets ((source → target) → arrow) = target : ForkTargets arrow
  ForkTargets (source → target) = '[]

fork ∷
  ∀ source targets arrows arrow.
  ( targets ~ ForkTargets arrow
  , source ~ ForkSource arrow
  , arrows ~ Fan source targets
  , arrows ~ Tuple (Inputs arrow (source → Tuple targets))
  , Currify (ToList arrows) (source → Tuple targets) arrow
  , Fork source targets
  ) ⇒
  arrow
fork = currify @(ToList arrows) fork_
