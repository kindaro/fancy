module Prelude.Fancy.Arithmetic where

import Prelude.Unicode

import Data.Bifunctor
import Data.Complex
import Data.Function
import Data.Functor.Compose
import Data.Functor.Const
import Data.Functor.Identity
import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as Map
import Data.Pointed
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tuple
import Test.Tasty.QuickCheck (Arbitrary)
import Witherable (Filterable)
import Witherable qualified

type Same ∷ [kind] → kind
type family Same stuff where
  Same '[thingie] = thingie
  Same (thingie : thingie : stuff) = Same (thingie : stuff)

type (×) = (,)
infixr 7 ×

(▵) ∷ (input → left) → (input → right) → input → left × right
function ▵ gunction = \input → (function input, gunction input)
infixr 7 ▵

type (+) = Either
infixr 6 +

(▿) ∷ (left → output) → (right → output) → left + right → output
function ▿ gunction = either function gunction
infixr 6 ▿

pattern (:×) ∷ left → right → left × right
pattern left :× right = (left, right)
infixr 3 :×
{-# COMPLETE (:×) #-}

-- Arrow in the category of finite sets and functions.
type key ⇸ value = Map key value
infixr 2 ⇸

-- Arrow in the category of finite sets and relations.
type this — that = Set (this, that)
infixr 3 —

-- The category of finite sets and relations is a dagger category.
dagger ∷ (Ord that, Ord this) ⇒ this — that → that — this
dagger = Set.map swap

newtype PointedSet value = PointedSet {pointedSet ∷ value × Set value}
  deriving (Show)
  deriving newtype (Arbitrary)
pointedSetToSet ∷ Ord value ⇒ PointedSet value → Set value
pointedSetToSet = uncurry Set.insert ∘ pointedSet
instance Ord value ⇒ Eq (PointedSet value) where (==) = (==) `on` pointedSetToSet
instance Ord value ⇒ Ord (PointedSet value) where compare = compare `on` pointedSetToSet
instance Pointed PointedSet where point = PointedSet ∘ (:× Set.empty)
pointedUnion ∷ Ord value ⇒ PointedSet value → PointedSet value → PointedSet value
pointedUnion (PointedSet (thisPoint :× thisSet)) (PointedSet (thatPoint :× thatSet)) = PointedSet do thisPoint :× Set.union thisSet (Set.insert thatPoint thatSet)

-- Target-wards adjunct.
power ∷ (Ord this, Ord that) ⇒ this — that → this ⇸ PointedSet that
power = Map.fromListWith pointedUnion . (fmap ∘ fmap) point . Set.toList

-- Source-wards adjunct.
unpower ∷ (Ord this, Ord that) ⇒ (this ⇸ PointedSet that) → (this — that)
unpower = Set.unions ∘ fmap (uncurry Set.map ∘ first (:×) ∘ fmap pointedSetToSet) ∘ Map.toList

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

eitherify ∷ Bool × α → α + α
eitherify (False :× value) = Left value
eitherify (True :× value) = Right value

booleanify ∷ α + α → Bool × α
booleanify (Left left) = False :× left
booleanify (Right right) = True :× right

simplify ∷ Complex α → α × α
simplify = realPart ▵ imagPart

complexify ∷ α × α → Complex α
complexify (α₁ :× α₂) = α₁ :+ α₂

partitionEithers ∷ Filterable filterable ⇒ filterable (left + right) → filterable left × filterable right
partitionEithers = Witherable.mapMaybe (either Just (const Nothing)) ▵ Witherable.mapMaybe (either (const Nothing) Just)

type I = Identity
type Δ = Const
type functor ∘ gunctor = Compose functor gunctor

pattern I ∷ α → I α
pattern I {i} = Identity {runIdentity = i}
{-# COMPLETE I #-}

pattern Δ ∷ α → Δ α β
pattern Δ {δ} = Const {getConst = δ}
{-# COMPLETE Δ #-}

-- | `W` for _«weld»_.
pattern W ∷ functor (gunctor α) → (functor ∘ gunctor) α
pattern W {w} = Compose {getCompose = w}

{-# COMPLETE W #-}

-- | This is the right tensorial strength of a functor.
deasil ∷ Functor functor ⇒ functor left × right → functor (left × right)
deasil (functor :× value) = fmap (:× value) functor

-- | This is the left tensorial strength of a functor.
widdershins ∷ Functor functor ⇒ left × functor right → functor (left, right)
widdershins (value :× functor) = fmap (value :×) functor

π₀ ∷ α × β → α
π₀ = fst

π₁ ∷ α × β → β
π₁ = snd
