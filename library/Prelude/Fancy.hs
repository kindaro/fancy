module Prelude.Fancy where

import Prelude.Unicode
import Data.Map.Lazy (Map)
import Data.Coerce

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

class Commutative bifunctor where
  commute ∷ bifunctor α (bifunctor β γ) → bifunctor β (bifunctor α γ)

instance Commutative (, ) where
  commute ∷ α × β × γ → β × α × γ
  commute (α, (β, γ)) = (β, (α, γ))

instance Commutative Either where
  commute ∷ α + β + γ → β + α + γ
  commute (Left value) = (Right ∘ Left) value
  commute (Right (Left value)) = Left value
  commute (Right (Right value)) = Right (Right value)

distribute ∷ Bool × α → α + α
distribute (False, value) = Left value
distribute (True, value) = Right value

type key ⇸ value = Map key value

type family Arguments arrow where
  Arguments (argument → result) = (argument, Arguments result)
  Arguments result = ( )

type family Result arrow where
  Result (argument → result) = Result result
  Result result = result

type family Inductive arrow = (boolean ∷ Bool) where
  Inductive (argument → result) = False
  Inductive result = True

class Inductive arrow ~ induction ⇒ TupleArrowAdjunction (induction ∷ Bool) arrow arguments result
  | arguments result → arrow, induction arrow → arguments, induction arrow → result
  where
    rightwards ∷ (arguments → result) → arrow
    leftwards ∷ arrow → arguments → result

instance Inductive result ~ True ⇒ TupleArrowAdjunction True result ( ) result
  where
    rightwards = ($ ( ))
    leftwards = const

instance {-# overlapping #-}
  ( TupleArrowAdjunction induction arrow arguments result
  , arguments ~ Arguments arrow, result ~ Result arrow )
  ⇒ TupleArrowAdjunction False (argument → arrow) (argument, arguments) result
  where
    rightwards function argument = rightwards (curry function argument)
    leftwards gunction (argument, arguments) = leftwards (gunction argument) arguments

newtype Arrow arrow = Arrow arrow

type family Loose arrows where
  Loose (Arrow arrow) = arrow
  Loose (function arrows moreArrows) = function (Loose arrows) (Loose moreArrows)
  Loose (function arrows) = Loose arrows

type family Source arrows = source where
  Source (Arrow (source -> target)) = source
  Source (Arrow (source -> target), arrows) = Same (Source arrows) source

type family Same someType someOtherType where
  Same someType someType = someType

type family Targets arrows = targets where
  Targets ( ) = ( )
  Targets (Arrow (source -> target), arrows) = (target, Targets arrows)

class Fork arrows where
  fork :: arrows -> Source arrows -> Targets arrows

instance Fork ( ) where
  fork ( ) = const ( )

instance (Fork arrows, source ~ Same (Source arrows) source, Source arrows ~ source) => Fork (Arrow (source -> target), arrows) where
  fork (arrow, arrows) source = (coerce arrow source, fork arrows source)

fork' :: Fork (a, (b, ( ))) => a -> b -> Source (a, (b, ( ))) -> Targets (a, (b, ( )))
fork' x y = fork (x, (y, ( )))

-- fork_ :: _ => _
-- fork_ = rightwards fork
