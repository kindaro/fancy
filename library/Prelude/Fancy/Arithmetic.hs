module Prelude.Fancy.Arithmetic where

import Prelude.Unicode

type left × right = (left, right)
infixr 7 ×

(▵) ∷ (input → left) → (input → right) → input → left × right
function ▵ gunction = \input → (function input, gunction input)
infixr 7 ▵

type left + right = Either left right
infixr 6 +

(▿) ∷ (left → output) → (right → output) → left + right → output
function ▿ gunction = either function gunction
infixr 6 ▿

pattern (:×) ∷ left → right → left × right
pattern left :× right = (left, right)
infixr 7 :×
{-# COMPLETE (:×) #-}

class Associative constructor where reassociate ∷ constructor α (constructor β γ) → constructor (constructor α β) γ
instance Associative (,) where reassociate (x :× y :× z) = (x :× y) :× z
instance Associative Either where
  reassociate (Left left) = (Left ∘ Left) left
  reassociate (Right (Left middle)) = (Left ∘ Right) middle
  reassociate (Right (Right right)) = Right right

class Commutative constructor where commute ∷ constructor α β → constructor β α
instance Commutative (,) where commute (left :× right) = right :× left
instance Commutative Either where
  commute (Left left) = Right left
  commute (Right right) = Left right

distribute ∷ Bool × α → α + α
distribute (False, value) = Left value
distribute (True, value) = Right value
