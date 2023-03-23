module Prelude.Fancy.Polymorphic.Polymorph where

import Prelude.Fancy.Arithmetic

import Data.Maybe

-- | A natural transformation.
data Polymorphic input output where Polymorphic ∷ (∀ value. input value → output value) → Polymorphic input output

examplePolymorphic ∷ Polymorphic [] Maybe
examplePolymorphic = Polymorphic listToMaybe

type Source ∷ ★ → (★ → ★)
type family Source arrow where
  Source (Polymorphic input output) = input

type Target ∷ ★ → (★ → ★)
type family Target arrow where
  Target (Polymorphic input output) = output

type family Arrow inputs outputs where
  Arrow (input value × ()) (output value × ()) = Polymorphic input output
  Arrow (input value × inputs) (output value × outputs) =
    Polymorphic
      (Same [input, Source (Arrow inputs outputs)])
      (Same [output, Target (Arrow inputs outputs)])

type family Input arrow output where
  Input (Polymorphic input output) () = ()
  Input (Polymorphic input output) (output value × outputs) =
    (input value × Input (Polymorphic input output) outputs)

type family Output arrow input where
  Output (Polymorphic input output) () = ()
  Output (Polymorphic input output) (input value × inputs) =
    (output value × Output (Polymorphic input output) inputs)

-- | You can map a natural transformation over a tuple.
class
  ( output ~ Output arrow input
  , input ~ Input arrow output
  , arrow ~ Arrow input output
  ) ⇒
  Polymorph arrow input output
  where
  polymorph ∷ arrow → input → output

instance {-# OVERLAPPING #-} Polymorph (Polymorphic input output) (input value × ()) (output value × ()) where
  polymorph (Polymorphic function) (value :× ()) = function value :× ()
instance
  ( Polymorph (Polymorphic input output) inputs outputs
  , Polymorphic input output ~ Arrow (input value × inputs) (output value × outputs)
  )
  ⇒ Polymorph (Polymorphic input output) (input value × inputs) (output value × outputs)
  where
  polymorph (Polymorphic function) (value :× values) = function value :× polymorph (Polymorphic function) values
