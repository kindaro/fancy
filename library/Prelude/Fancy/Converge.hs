module Prelude.Fancy.Converge where

import Prelude.Unicode
import Prelude

import Prelude.Fancy.Arithmetic

converge ∷ Eq α ⇒ [α] → [α]
converge = convergeBy (≡)

fixedPoint ∷ Eq α ⇒ (α → α) → α → α
fixedPoint process = last ∘ converge ∘ iterate process

convergeBy ∷ (α → α → Bool) → [α] → [α]
convergeBy _ [] = []
convergeBy _ [x] = [x]
convergeBy equality (x : xs@(y : _))
  | x `equality` y = [x]
  | otherwise = x : convergeBy equality xs

fixedPointBy ∷ (α → α → Bool) → (α → α) → α → α
fixedPointBy equality function = last ∘ convergeBy equality ∘ iterate function

differences ∷ [α] → [α × α]
differences list = zip list (tail list)
