module Prelude.Fancy.Polymorphic.Bloom where

import Prelude.Fancy.Arithmetic
import Prelude.Fancy.Assorti

-- | A cone from the _Δ_ functor. It can only build empty containers. Pretty and vain, like blooming flowers.
data Blooming input output where Blooming ∷ (∀ value. input → output value) → Blooming input output

exampleBlooming ∷ Blooming input Maybe
exampleBlooming = Blooming (constant Nothing)

type Source ∷ ★ → ★
type family Source arrow where
  Source (Blooming input output) = input

type Target ∷ ★ → (★ → ★)
type family Target arrow where
  Target (Blooming input output) = output

type family Arrow inputs outputs where
  Arrow (input × ()) (output value × ()) = Blooming input output
  Arrow (input × inputs) (output value × outputs) =
    Blooming
      (Same [input, Source (Arrow inputs outputs)])
      (Same [output, Target (Arrow inputs outputs)])

type family Input arrow output where
  Input (Blooming input output) (output value × ()) = input × ()
  Input (Blooming input output) (output value × outputs) =
    input × Input (Blooming input output) outputs

class
  ( input ~ Input arrow output
  , arrow ~ Arrow input output
  ) ⇒
  Bloom arrow input output
  where
  bloom ∷ arrow → input → output

instance {-# OVERLAPPING #-} Bloom (Blooming input output) (input × ()) (output value × ()) where
  bloom (Blooming function) (value :× ()) = function value :× ()
instance
  ( Bloom (Blooming input output) inputs outputs
  , (input × inputs) ~ Input (Blooming input output) (output value × outputs)
  , Blooming input output ~ Arrow (input × inputs) (output value × outputs)
  ) ⇒
  Bloom (Blooming input output) (input × inputs) (output value × outputs)
  where
  bloom (Blooming function) (value :× values) = function value :× bloom (Blooming function) values
