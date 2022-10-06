{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Prelude.Fancy.Polymorphic.Warp.Checks where

import Prelude.Fancy.Arithmetic
import Prelude.Fancy.Polymorphic.Warp

checkWarp₁ ∷ [Bool] × [Char] × ()
checkWarp₁ = warp exampleWarping (True :× 'a' :× ())

checkWarp₂ ∷ _
checkWarp₂ = warp exampleWarping (True :× 'a' :× ())

checkWarp₃ ∷ [Bool] × [Char] × ()
checkWarp₃ = warp exampleWarping _

checkWarp₄ ∷ [Bool] × [Char] × ()
checkWarp₄ = warp _ (True :× 'a' :× ())
