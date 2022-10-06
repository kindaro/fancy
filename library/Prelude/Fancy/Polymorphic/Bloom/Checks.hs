{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Prelude.Fancy.Polymorphic.Bloom.Checks where

import Prelude.Fancy.Arithmetic
import Prelude.Fancy.Polymorphic.Bloom

checkBlooming₁ ∷ Maybe Char × Maybe Int × ()
checkBlooming₁ = bloom (exampleBlooming @Bool) (True :× False :× ())

checkBlooming₂ ∷ Maybe Char × Maybe Int × ()
checkBlooming₂ = bloom (exampleBlooming @Bool) _

checkBlooming₃ ∷ Maybe Char × Maybe Int × ()
checkBlooming₃ = bloom _ (True :× False :× ())
