module Prelude.Fancy.Polymorphic.Warp where

import Prelude.Fancy.Arithmetic

-- | A transformation from the identity functor. It can create containers of any form.
data Warping output where Warping ∷ (∀ value. value → output value) → Warping output

exampleWarping ∷ Warping []
exampleWarping = Warping (replicate 3)

type Target ∷ ★ → (★ → ★)
type family Target arrow where
  Target (Warping output) = output

type family Arrow inputs outputs where
  Arrow (value × ()) (output value × ()) = Warping output
  Arrow (value × inputs) (output value × outputs) =
    Warping
      (Same [output, Target (Arrow inputs outputs)])

type family Input arrow output where
  Input (Warping output) (output value × ()) = value × ()
  Input (Warping output) (output value × outputs) =
    (value × Input (Warping output) outputs)

type family Output arrow input where
  Output (Warping output) () = ()
  Output (Warping output) (value × inputs) =
    (output value × Output (Warping output) inputs)

-- | You can map a natural transformation over a tuple.
class
  ( output ~ Output arrow input
  , input ~ Input arrow output
  , arrow ~ Arrow input output
  ) ⇒
  Warp arrow input output
  where
  warp ∷ arrow → input → output

instance {-# OVERLAPPING #-} Warp (Warping output) (value × ()) (output value × ()) where
  warp (Warping function) (value :× ()) = function value :× ()
instance
  ( Warp (Warping output) inputs outputs
  , Warping output ~ Arrow (value × inputs) (output value × outputs)
  , (value × inputs) ~ Input (Warping output) (output value × outputs)
  )
  ⇒ Warp (Warping output) (value × inputs) (output value × outputs)
  where
  warp (Warping function) (value :× values) = function value :× warp (Warping function) values
