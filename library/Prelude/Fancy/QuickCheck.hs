module Prelude.Fancy.QuickCheck
  ( module Test.Tasty
  , module Test.Tasty.QuickCheck
  , ExtensionalEquality (..)
  , (↔)
  , splitting
  , isomorphosis
  ) where

import Prelude.Fancy

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Text qualified as Text

class ExtensionalEquality α where
  isExtensionallyEqual ∷ α → α → Property

instance {-# OVERLAPPABLE #-} (Eq α, Show α, Arbitrary α) ⇒ ExtensionalEquality α where
  isExtensionallyEqual x y = property $ x === y

instance (ExtensionalEquality β, Show α, Arbitrary α) ⇒ ExtensionalEquality (α → β) where
  isExtensionallyEqual f g = property \x → f x ↔ g x

instance (ExtensionalEquality β, Show α, Arbitrary α) ⇒ ExtensionalEquality (Fun α β) where
  isExtensionallyEqual (applyFun → f) (applyFun → g) = property \x → f x ↔ g x

infix 4 ↔
(↔) ∷ ExtensionalEquality α ⇒ α → α → Property
(↔) = isExtensionallyEqual

splitting
  ∷ (ExtensionalEquality (sliver → sliver), ExtensionalEquality (whole → whole))
  ⇒ Text
  → (whole → sliver)
  → (sliver → whole)
  → TestTree
splitting name overlay inlay = testProperty (unwords ["split", Text.unpack name]) do overlay ∘ inlay ↔ id

isomorphosis ∷ (ExtensionalEquality (α → α), ExtensionalEquality (β → β)) ⇒ Text → (α → β) → (β → α) → TestTree
isomorphosis name there back = writ (testGroup (unwords ["isomorphosis", Text.unpack name])) do
  say do splitting "α → β → α" there back
  say do splitting "β → α → β" back there
