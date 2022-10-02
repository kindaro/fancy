{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Prelude.Fancy where

import Prelude.Unicode
import Data.Map.Lazy (Map)
import Data.Kind

bind ∷ Monad monad ⇒ (input → monad output) → monad input → monad output
bind = (=<<)

type left × right = (left, right)
infixr 7 ×

(▵) ∷ (input → left) → (input → right) → input → (left, right)
function ▵ gunction = \ input → (function input, gunction input)
infixr 7 ▵

type left + right = Either left right
infixr 6 +

(▿) ∷ (left → output) → (right → output) → left + right → output
function ▿ gunction = either function gunction
infixr 6 ▿

pattern (:×) ∷ left → right → left × right
pattern left :× right = (left, right)
infixr 7 :×
{-# complete (:×) #-}

class Associative constructor where reassociate ∷ constructor α (constructor β γ) → constructor (constructor α β) γ
instance Associative (, ) where reassociate (x :× y :× z) = (x :× y) :× z
instance Associative Either where
  reassociate (Left left) = (Left ∘ Left) left
  reassociate (Right (Left middle)) = (Left ∘ Right) middle
  reassociate (Right (Right right)) = Right right

class Commutative constructor where commute ∷ constructor α β → constructor β α
instance Commutative (, ) where commute (left :× right) = right :× left
instance Commutative Either where
  commute (Left left) = Right left
  commute (Right right) = Left right

distribute ∷ Bool × α → α + α
distribute (False, value) = Left value
distribute (True, value) = Right value

type key ⇸ value = Map key value

type family Tuple (stuff ∷ [★]) = tuple | tuple → stuff where
  Tuple '[ ] = ( )
  Tuple (thingie: stuff) = thingie × Tuple stuff

type family Arrow (inputs ∷ [★]) output where
  Arrow '[ ] output = output
  Arrow (input: inputs) output = input → Arrow inputs output

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
  , input : inputs ~ Inputs (input → arrow) output
  , output ~ Output (input → arrow) (input : inputs)
  ) ⇒
  Currify (input : inputs) output (input → arrow)
  where
  currify function = \input → currify \inputs → function (input :× inputs)
  uncurrify arrow = \(input :× inputs) → uncurrify (arrow input) inputs

-- * checks for 'currify'

-- | Everything can be inferred if arity is zero.
currifyTypeChecks₁ ∷ _
currifyTypeChecks₁ = currify @'[ ]

-- | Everything can be inferred from the length of the tuple and the output.
currifyTypeChecks₂ ∷ _
currifyTypeChecks₂ = currify @_ @Bool (undefined ∷ _ × _ × _ × ( ) → _)

-- | Everything can be inferred from arity and output.
currifyTypeChecks₃ ∷ _ → _ → _ × _
currifyTypeChecks₃ = currify @_ @(Bool × Char) undefined

-- | Everything can be inferred from arity and inputs.
currifyTypeChecks₄ ∷ _ → _ → _ × _
currifyTypeChecks₄ = currify @'[Bool, Char] undefined

-- * checks for 'uncurrify'

-- | Everything can be inferred if arity is zero.
uncurrifyTypeChecks₁ ∷ _
uncurrifyTypeChecks₁ = uncurrify @'[ ]

-- | Everything can be inferred from the length of the tuple and the output.
uncurrifyTypeChecks₂ ∷ _ × _ × _ × ( ) → _
uncurrifyTypeChecks₂ = uncurrify @_ @Bool undefined

-- | Everything can be inferred from arity and output.
uncurrifyTypeChecks₃ ∷ _
uncurrifyTypeChecks₃ = uncurrify @_ @(Bool × Char) (undefined ∷ _ → _ → _ → _ × _)

-- | Everything can be inferred from arity and inputs.
uncurrifyTypeChecks₄ ∷ _
uncurrifyTypeChecks₄ = uncurrify @'[Bool, Char] (undefined ∷ _ → _ → _ → _ × _)

-- forkTypeChecks₁ ∷ (Char, ( ))
-- forkTypeChecks₁ = fork_ id 'c'

-- forkTypeChecks₂ ∷ (Char, (Char, ( )))
-- forkTypeChecks₂ = fork_ id id 'c'

-- forkTypeChecksNoSignature₁ ∷ _
-- forkTypeChecksNoSignature₁ = fork_ (id ∷ Char → Char) 'c'

-- forkTypeChecksNoSignature₂ ∷ _
-- forkTypeChecksNoSignature₂ = fork_ id id 'c'

class TupleToList α β where tupleToList ∷ α → [β]
instance TupleToList ( ) β where tupleToList ( ) = [ ]
instance TupleToList γ α ⇒ TupleToList (α, γ) α where tupleToList (x, xs) = x: tupleToList xs
