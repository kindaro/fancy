module Prelude.Fancy.Polymorphic.Burst where

import Prelude.Fancy.Arithmetic

-- | A transformation to the identity functor. It can only observe the contents of a container.
data Bursting input where Bursting ∷ (∀ value. input value → value) → Bursting input

exampleBursting ∷ Bursting ((->) Bool)
exampleBursting = Bursting ($ True)

type Source ∷ ★ → (★ → ★)
type family Source arrow where
  Source (Bursting input) = input

type family Arrow inputs outputs where
  Arrow (input value × ()) (value × ()) = Bursting input
  Arrow (input value × inputs) (value × outputs) =
    Bursting
      (Same [input, Source (Arrow inputs outputs)])

type family Input arrow output where
  Input (Bursting input) () = ()
  Input (Bursting input) (value × outputs) =
    (input value × Input (Bursting input) outputs)

type family Output arrow input where
  Output (Bursting input) () = ()
  Output (Bursting input) (input value × inputs) =
    (value × Output (Bursting input) inputs)

-- | You can map a natural transformation over a tuple.
class
  ( output ~ Output arrow input
  , input ~ Input arrow output
  , arrow ~ Arrow input output
  ) ⇒
  Burst arrow input output
  where
  burst ∷ arrow → input → output

instance {-# OVERLAPPING #-} Burst (Bursting input) (input value × ()) (value × ()) where
  burst (Bursting function) (value :× ()) = function value :× ()
instance
  ( Burst (Bursting input) inputs outputs
  , Bursting input ~ Arrow (input value × inputs) (value × outputs)
  ) ⇒
  Burst (Bursting input) (input value × inputs) (value × outputs)
  where
  burst (Bursting function) (value :× values) = function value :× burst (Bursting function) values
