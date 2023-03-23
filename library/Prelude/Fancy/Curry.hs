module Prelude.Fancy.Curry where

import Data.Kind
import Prelude.Fancy.Arithmetic

type family Arrow (inputs ∷ [★]) output where
  Arrow '[] output = output
  Arrow (input : inputs) output = input → Arrow inputs output

type family Inputs arrow output where
  Inputs output output = '[]
  Inputs (input → arrow) output = input : Inputs arrow output
  Inputs (functor _) (functor _) = '[]
  Inputs (functor _ _) (functor _ _) = '[]

type family Output arrow (inputs ∷ [★]) where
  Output (input → arrow) (input : inputs) = Output arrow inputs
  Output output '[] = output

type Currify ∷ [★] → ★ → ★ → Constraint
class
  ( inputs ~ Inputs arrow output
  , output ~ Output arrow inputs
  , arrow ~ Arrow inputs output
  ) ⇒
  Currify inputs output arrow
  where
  currify ∷ (Tuple inputs → output) → arrow
  uncurrify ∷ arrow → Tuple inputs → output

instance Currify '[] output output where
  currify spike = spike ()
  uncurrify output = \() → output

instance
  ( Currify inputs output arrow
  , (input : inputs) ~ Inputs (input → arrow) output
  , output ~ Output (input → arrow) (input : inputs)
  )
  ⇒ Currify (input : inputs) output (input → arrow)
  where
  currify function = \input → currify \inputs → function (input :× inputs)
  uncurrify arrow = \(input :× inputs) → uncurrify (arrow input) inputs
