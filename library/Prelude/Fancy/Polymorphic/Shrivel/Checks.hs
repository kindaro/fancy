{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Prelude.Fancy.Polymorphic.Shrivel.Checks where

import Prelude.Fancy.Arithmetic
import Prelude.Fancy.Polymorphic.Shrivel

checkShrivelling₁ ∷ Int × Int × ()
checkShrivelling₁ = shrivel exampleShrivelling ([False, True] :× ['a', 'b', 'c'] :× ())

checkShrivelling₂ ∷ _
checkShrivelling₂ = shrivel exampleShrivelling ([False, True] :× ['a', 'b', 'c'] :× ())

checkShrivelling₃ ∷ Int × Int × ()
checkShrivelling₃ = shrivel _ ([False, True] :× ['a', 'b', 'c'] :× ())
