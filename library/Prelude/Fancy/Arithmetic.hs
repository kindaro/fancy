module Prelude.Fancy.Arithmetic where

import Prelude.Unicode

import Data.Functor.Compose
import Data.Functor.Const
import Data.Functor.Identity
import Witherable (Filterable)
import Witherable qualified

type Same ∷ [kind] → kind
type family Same stuff where
  Same '[thingie] = thingie
  Same (thingie : thingie : stuff) = Same (thingie : stuff)

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

type family Tuple (stuff ∷ [★]) = tuple | tuple → stuff where
  Tuple '[] = ()
  Tuple (thingie : stuff) = thingie × Tuple stuff

type ToList ∷ ★ → [★]
type family ToList tuple = result | result → tuple where
  ToList (thingie × stuff) = thingie : ToList stuff
  ToList () = '[]

class β ~ Same (ToList α) ⇒ TupleToList α β where tupleToList ∷ α → [β]
instance {-# OVERLAPPING #-} TupleToList (α, ()) α where tupleToList (x, ()) = [x]
instance (α ~ Same (α : ToList γ), TupleToList γ α) ⇒ TupleToList (α, γ) α where tupleToList (x, xs) = x : tupleToList xs

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

partitionEithers ∷ Filterable filterable ⇒ filterable (left + right) → filterable left × filterable right
partitionEithers = Witherable.mapMaybe (either Just (const Nothing)) ▵ Witherable.mapMaybe (either (const Nothing) Just)

type I = Identity
type Δ = Const
type functor ∘ gunctor = Compose functor gunctor

pattern I ∷ α → I α
pattern I{i} = Identity{runIdentity = i}
{-# COMPLETE I #-}

pattern Δ ∷ α → Δ α β
pattern Δ{δ} = Const{getConst = δ}
{-# COMPLETE Δ #-}

-- | `W` for _«weld»_.
pattern W ∷ functor (gunctor α) → (functor ∘ gunctor) α
pattern W{w} = Compose{getCompose = w}

{-# COMPLETE W #-}
