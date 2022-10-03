{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Prelude.Fancy.Curry.Checks where

import Prelude.Fancy.Arithmetic
import Prelude.Fancy.Curry

-- * checks for 'currify'

-- | Everything can be inferred if arity is zero.
currifyTypeChecks₁ ∷ _
currifyTypeChecks₁ = currify @'[ ]

-- | Everything can be inferred from the length of the tuple and the output.
currifyTypeChecks₂ ∷ _
currifyTypeChecks₂ = currify @_ @Bool (undefined ∷ _ × _ × _ × ( ) → _)

-- | Everything can be inferred from arity and output.
currifyTypeChecks₃ ∷ _ → _ → _ × _
currifyTypeChecks₃ = currify @_ @(Bool × Char) undefined

-- | Everything can be inferred from arity and inputs.
currifyTypeChecks₄ ∷ _ → _ → _ × _
currifyTypeChecks₄ = currify @'[Bool, Char] undefined

-- * checks for 'uncurrify'

-- | Everything can be inferred if arity is zero.
uncurrifyTypeChecks₁ ∷ _
uncurrifyTypeChecks₁ = uncurrify @'[ ]

-- | Everything can be inferred from the length of the tuple and the output.
uncurrifyTypeChecks₂ ∷ _ × _ × _ × ( ) → _
uncurrifyTypeChecks₂ = uncurrify @_ @Bool undefined

-- | Everything can be inferred from arity and output.
uncurrifyTypeChecks₃ ∷ _
uncurrifyTypeChecks₃ = uncurrify @_ @(Bool × Char) (undefined ∷ _ → _ → _ → _ × _)

-- | Everything can be inferred from arity and inputs.
uncurrifyTypeChecks₄ ∷ _
uncurrifyTypeChecks₄ = uncurrify @'[Bool, Char] (undefined ∷ _ → _ → _ → _ × _)
