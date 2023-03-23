module Main where

import Prelude.Fancy
import Prelude.Fancy.QuickCheck

import Data.Maybe
import Data.Word

main ∷ IO ()
main = (defaultMain ∘ writ (testGroup "")) do
  say do testProperty "picky singleton" do isJust ∘ picky @() ↔ (≡ 1) ∘ length
  say do testProperty "reckon is full" do \x → x `elem` (reckon @Word8)
  say do isomorphosis "eitherify booleanify" eitherify (booleanify @Word8)
  say do isomorphosis "simplify complexify" simplify (complexify @Word8)
  say do isomorphosis @(Word8 — Word8) "power unpower" power unpower
