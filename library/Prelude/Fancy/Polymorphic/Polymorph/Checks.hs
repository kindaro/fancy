{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Prelude.Fancy.Polymorphic.Polymorph.Checks where

import Prelude.Fancy.Arithmetic
import Prelude.Fancy.Polymorphic.Polymorph

checkPolymorphic₁ ∷ Maybe Bool × Maybe Char × ()
checkPolymorphic₁ = polymorph examplePolymorphic ([False, True] :× ['a', 'b', 'c'] :× ())

checkPolymorphic₂ ∷ _
checkPolymorphic₂ = polymorph examplePolymorphic ([False, True] :× ['a', 'b', 'c'] :× ())

checkPolymorphic₃ ∷ Maybe Bool × Maybe Char × ()
checkPolymorphic₃ = polymorph examplePolymorphic _

checkPolymorphic₄ ∷ Maybe Bool × Maybe Char × ()
checkPolymorphic₄ = polymorph _ ([False, True] :× ['a', 'b', 'c'] :× ())
