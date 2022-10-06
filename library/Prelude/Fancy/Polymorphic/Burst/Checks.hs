{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Prelude.Fancy.Polymorphic.Burst.Checks where

import Prelude.Fancy.Arithmetic
import Prelude.Fancy.Polymorphic.Burst

checkBurst₁ ∷ Bool × Int × ()
checkBurst₁ = burst exampleBursting (not :× fromEnum @Bool :× ())

checkBurst₂ ∷ _
checkBurst₂ = burst exampleBursting (not :× fromEnum @Bool :× ())

checkBurst₃ ∷ Bool × Int × ()
checkBurst₃ = burst exampleBursting _

checkBurst₄ ∷ Bool × Int × ()
checkBurst₄ = burst _ (not :× fromEnum @Bool :× ())
