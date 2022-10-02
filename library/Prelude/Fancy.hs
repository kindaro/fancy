{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
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

class Associative constructor where reassociate ∷ constructor α (constructor β γ) → constructor (constructor α β) γ
instance Associative (, ) where reassociate (x, (y, z)) = ((x, y), z)
instance Associative Either where
  reassociate (Left left) = (Left ∘ Left) left
  reassociate (Right (Left middle)) = (Left ∘ Right) middle
  reassociate (Right (Right right)) = Right right

class Commutative constructor where commute ∷ constructor α β → constructor β α
instance Commutative (, ) where commute (left, right) = (right, left)
instance Commutative Either where
  commute (Left left) = Right left
  commute (Right right) = Left right

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
  Inductive (argument → result) = True
  Inductive result = False

class Inductive arrow ~ induction ⇒ TupleArrowAdjunction (induction ∷ Bool) arrow arguments result
  | arguments result → arrow, induction arrow → arguments, induction arrow → result
  where
    rightwards ∷ (arguments → result) → arrow
    leftwards ∷ arrow → arguments → result

instance Inductive result ~ False ⇒ TupleArrowAdjunction False result ( ) result
  where
    rightwards = ($ ( ))
    leftwards = const

instance {-# overlapping #-}
  ( TupleArrowAdjunction induction arrow arguments result
  , arguments ~ Arguments arrow, result ~ Result arrow )
  ⇒ TupleArrowAdjunction True (argument → arrow) (argument, arguments) result
  where
    rightwards function argument = rightwards (curry function argument)
    leftwards gunction (argument, arguments) = leftwards (gunction argument) arguments

newtype Arrow arrow = Arrow arrow

type family Loose arrows where
  Loose (Arrow arrow) = arrow
  Loose (function arrows moreArrows) = function (Loose arrows) (Loose moreArrows)
  Loose (function arrows) = Loose arrows

type family Source arrows where
  Source (source → target, ( )) = source
  Source (source → target, arrows) = Same (Source arrows) source

type family Same someType someOtherType where
  Same someType someType = someType

type family Targets arrows = targets where
  Targets ( ) = ( )
  Targets (source → target, arrows) = (target, Targets arrows)

class Fork arrows source targets | arrows → source targets, source targets → arrows where
  fork :: arrows -> source -> targets

instance {-# overlapping #-} Fork (source → target, ( )) source (target, ( )) where
  fork (function, ( )) input = (function input, ( ))

instance (Fork arrows source targets, source ~ Same (Source arrows) source, Source arrows ~ source, Source (source → target, arrows) ~ source, targets ~ Targets arrows) => Fork (source -> target, arrows) source (target, targets) where
  fork (arrow, arrows) source = (coerce arrow source, fork arrows source)

fork' :: Fork (a, (b, ( ))) (Source (a, (b, ( )))) (Targets (a, (b, ( )))) => a -> b -> Source (a, (b, ( ))) -> Targets (a, (b, ( )))
fork' x y = fork (x, (y, ( )))

type family AppendFamily there this where
  AppendFamily ( ) this = (this, ( ))
  AppendFamily (that, there) this = (that, AppendFamily there this)

class Append there this where append ∷ there → this → AppendFamily there this
instance Append ( ) this where append ( ) this = (this, ( ))
instance Append there this ⇒ Append (that, there) this where append (that, there) this = (that, append there this)

type family Curried arguments result where
  Curried ( ) result = result
  Curried (argument, arguments) result = argument → Curried arguments result

type Reverse ∷ ★ → ★
type Reverse tuple = ReverseFamily tuple ( )
type family ReverseFamily (straight ∷ ★) (backwards ∷ ★) ∷ ★ where
  ReverseFamily ( ) backwards = backwards
  ReverseFamily (thingie, straight) backwards = ReverseFamily straight (thingie, backwards)
class ReverseTailRecursive straight backwards result | straight backwards → result, result straight → backwards where reverseTailRecursive ∷ straight → backwards → result
instance ReverseTailRecursive ( ) backwards backwards where reverseTailRecursive ( ) backwards = backwards
instance (ReverseTailRecursive straight (thingie, backwards) result)
  ⇒ ReverseTailRecursive (thingie, straight) backwards result
  where reverseTailRecursive (thingie, straight) backwards = reverseTailRecursive straight (thingie, backwards)
reverseTuple ∷ ReverseTailRecursive tuple ( ) (Reverse tuple) ⇒ tuple → Reverse tuple
reverseTuple tuple = reverseTailRecursive tuple ( )

type family Leftie binaryOperation where Leftie (constructor left right) = left
type family Rightie binaryOperation where Rightie (constructor left right) = right

type family Fan source targets where
  Fan source ( ) = ( )
  Fan source (target, targets) = (source → target, Fan source targets)

type family ExtendRange arrow target = result | result → arrow target where
  ExtendRange (argument → arrow) target = argument → ExtendRange arrow target
  ExtendRange targets target = (target, targets)

type family InductiveFork_ arrow where
  InductiveFork_ ((source → target) → arrow) = True
  InductiveFork_ (source → ( )) = False

class Fork_ (inductive ∷ Bool) arrow source targets | source targets → arrow, inductive arrow → source, inductive arrow → targets
instance {-# overlapping #-} Fork_ False (source → ( )) source ( )
instance (Fork_ (InductiveFork_ arrow) arrow source targets, widerArrow ~ ExtendRange arrow target)
  ⇒ Fork_ True ((source → target) → widerArrow) source (target, targets)

fork_ ∷ ∀ arrow arguments arrows source targets.
  ( arguments ~ Arguments arrow
  , arrows ~ Reverse (Rightie (Reverse arguments))
  , targets ~ Targets arrows
  , Result arrow ~ targets
  , source ~ Source arrows
  , source ~ Leftie (Reverse arguments)
  , arrow ~ Curried arguments targets
  , arrows ~ Fan source targets
  , arguments ~ AppendFamily arrows source
  , Fork_ True arrow source targets
  , Reverse arguments
      ~ ( Leftie (Reverse arguments)
        , Rightie (Reverse arguments)
        )
  , TupleArrowAdjunction True arrow arguments targets
  , ReverseTailRecursive arguments () (Reverse arguments)
  , ReverseTailRecursive (Rightie (Reverse arguments)) () (Reverse (Rightie (Reverse arguments)))
  , Fork arrows source (Targets arrows)
  ) ⇒
  arrow
fork_ = rightwards function
  where
    function ∷ arguments → Targets (Reverse (Rightie (Reverse arguments)))
    function arrowsAndArgument =
      let (argument, reverseArrows) = reverseTuple arrowsAndArgument
      in fork @arrows @(Source arrows) @(Targets arrows) (reverseTuple reverseArrows) argument

forkTypeChecks₁ ∷ (Char, ( ))
forkTypeChecks₁ = fork_ id 'c'

forkTypeChecks₂ ∷ (Char, (Char, ( )))
forkTypeChecks₂ = fork_ id id 'c'

forkTypeChecksNoSignature₁ ∷ (_, _)
forkTypeChecksNoSignature₁ = fork_ (id ∷ Char → Char) 'c'

forkTypeChecksNoSignature₂ ∷ (_, _)
forkTypeChecksNoSignature₂ = fork_ id id 'c'

class TupleToList α β where tupleToList ∷ α → [β]
instance TupleToList ( ) β where tupleToList ( ) = [ ]
instance TupleToList γ α ⇒ TupleToList (α, γ) α where tupleToList (x, xs) = x: tupleToList xs
