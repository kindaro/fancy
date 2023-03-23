module Prelude.Fancy.Polymorphic.Shrivel where

import Prelude.Fancy.Arithmetic

-- | A cone to the _Δ_ functor. It can only observe the shape of a container.
data Shrivelling input output where Shrivelling ∷ (∀ value. input value → output) → Shrivelling input output

exampleShrivelling ∷ Shrivelling [] Int
exampleShrivelling = Shrivelling length

type Source ∷ ★ → (★ → ★)
type family Source arrow where
  Source (Shrivelling input output) = input

type Target ∷ ★ → ★
type family Target arrow where
  Target (Shrivelling input output) = output

type family Arrow inputs outputs where
  Arrow (input value × ()) (output × ()) = Shrivelling input output
  Arrow (input value × inputs) (output × outputs) =
    Shrivelling
      (Same [input, Source (Arrow inputs outputs)])
      (Same [output, Target (Arrow inputs outputs)])

type family Output arrow input where
  Output (Shrivelling input output) () = ()
  Output (Shrivelling input output) (input value × inputs) =
    (output × Output (Shrivelling input output) inputs)

class
  ( output ~ Output arrow input
  , arrow ~ Arrow input output
  ) ⇒
  Shrivel arrow input output
  where
  shrivel ∷ arrow → input → output

instance {-# OVERLAPPING #-} Shrivel (Shrivelling input output) (input value × ()) (output × ()) where
  shrivel (Shrivelling function) (value :× ()) = function value :× ()
instance
  ( Shrivel (Shrivelling input output) inputs outputs
  , (output × outputs) ~ Output (Shrivelling input output) (input value × inputs)
  , Shrivelling input output ~ Arrow (input value × inputs) (output × outputs)
  )
  ⇒ Shrivel (Shrivelling input output) (input value × inputs) (output × outputs)
  where
  shrivel (Shrivelling function) (value :× values) = function value :× shrivel (Shrivelling function) values
