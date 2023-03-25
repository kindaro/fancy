{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-tabs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveTraversable #-}
module Main where

import Prelude.Fancy
import Prelude.Fancy.QuickCheck

-- import Data.Maybe
import Data.Functor.Classes
import Data.Proxy
import Data.Typeable
import GHC.Generics

main ∷ IO ()
main = (defaultMain ∘ writ (testGroup "")) do
  -- say do testProperty "picky singleton" do isJust ∘ picky @() ↔ (≡ 1) ∘ length
  -- say do testProperty "reckon is full" do \x → x `elem` (reckon @Bool)
  -- say do isomorphosis "eitherify booleanify" eitherify (booleanify @Bool)
  -- say do isomorphosis "simplify complexify" simplify (complexify @Bool)
  -- say do isomorphosis @(Bool — Bool) "power unpower" power unpower

  -- say do monadicity @(Δ ( )) @Bool
  -- say do monadicity @((×) Bool) @Bool
  -- say do monadicity @Maybe @Bool
  -- say do monadicity @((→) Bool) @Bool
  -- say do monadicity @[ ] @Bool

  say do
    writ (testGroup "swap") do
      -- say do swap @[ ] @[ ] @Bool @Bool
      say do swap @[ ] @((×) [Bool]) @Bool @Bool
      say do swap @[ ] @((×) Bool) @Bool @Bool
      say do swap @[ ] @Maybe @Bool @Bool
      say do swap @[ ] @(Δ ( )) @Bool @Bool
      say do swap @((×) [Bool]) @[ ] @Bool @Bool
      say do swap @((×) [Bool]) @((×) [Bool]) @Bool @Bool
      say do swap @((×) [Bool]) @((×) Bool) @Bool @Bool
      say do swap @((×) [Bool]) @Maybe @Bool @Bool
      say do swap @((×) [Bool]) @(Δ ( )) @Bool @Bool
      say do swap @((×) Bool) @[ ] @Bool @Bool
      say do swap @((×) Bool) @((×) [Bool]) @Bool @Bool
      say do swap @((×) Bool) @((×) Bool) @Bool @Bool
      say do swap @((×) Bool) @Maybe @Bool @Bool
      say do swap @((×) Bool) @(Δ ( )) @Bool @Bool
      say do swap @Maybe @[ ] @Bool @Bool
      say do swap @Maybe @((×) [Bool]) @Bool @Bool
      say do swap @Maybe @((×) Bool) @Bool @Bool
      say do swap @Maybe @Maybe @Bool @Bool
      say do swap @Maybe @(Δ ( )) @Bool @Bool
      say do swap @(Δ ( )) @[ ] @Bool @Bool
      say do swap @(Δ ( )) @((×) [Bool]) @Bool @Bool
      say do swap @(Δ ( )) @((×) Bool) @Bool @Bool
      say do swap @(Δ ( )) @Maybe @Bool @Bool
      say do swap @(Δ ( )) @(Δ ( )) @Bool @Bool

  say do
    writ (testGroup "monadicity") do
      -- say do monadicity @([ ] 	∘ [ ]) @Bool
      say do monadicity @((×) [Bool] 	∘ [ ]) @Bool
      say do monadicity @((×) Bool 	∘ [ ]) @Bool
      say do monadicity @((→) Bool 	∘ [ ]) @Bool
      say do monadicity @(Maybe 	∘ [ ]) @Bool
      say do monadicity @(Δ ( ) 	∘ [ ]) @Bool
      say do monadicity @([ ] 	∘ (×) [Bool]) @Bool
      say do monadicity @((×) [Bool] 	∘ (×) [Bool]) @Bool
      say do monadicity @((×) Bool 	∘ (×) [Bool]) @Bool
      say do monadicity @((→) Bool 	∘ (×) [Bool]) @Bool
      say do monadicity @(Maybe 	∘ (×) [Bool]) @Bool
      say do monadicity @(Δ ( ) 	∘ (×) [Bool]) @Bool
      say do monadicity @([ ] 	∘ (×) Bool) @Bool
      say do monadicity @((×) [Bool] 	∘ (×) Bool) @Bool
      say do monadicity @((×) Bool 	∘ (×) Bool) @Bool
      say do monadicity @((→) Bool 	∘ (×) Bool) @Bool
      say do monadicity @(Maybe 	∘ (×) Bool) @Bool
      say do monadicity @(Δ ( ) 	∘ (×) Bool) @Bool
      say do monadicity @((×) [Bool] 	∘ Maybe) @Bool
      say do monadicity @((×) Bool 	∘ Maybe) @Bool
      say do monadicity @((→) Bool 	∘ Maybe) @Bool
      say do monadicity @([ ] 	∘ Maybe) @Bool
      say do monadicity @(Maybe 	∘ Maybe) @Bool
      say do monadicity @(Δ ( ) 	∘ Maybe) @Bool
      say do monadicity @((×) [Bool] 	∘ Δ ( )) @Bool
      say do monadicity @((×) Bool 	∘ Δ ( )) @Bool
      say do monadicity @((→) Bool 	∘ Δ ( )) @Bool
      say do monadicity @(Maybe 	∘ Δ ( )) @Bool
      say do monadicity @([ ] 	∘ Δ ( )) @Bool
      say do monadicity @(Δ ( ) 	∘ Δ ( )) @Bool

instance (Enum source, Bounded source, Show source, Show target) ⇒ Show (source → target) where show f = Prelude.show do for reckon \ x → (x, f x)

unity, associativity, monadicity ∷
  ∀ (m ∷ ★ → ★) (a ∷ ★)
  . (Monad m, Arbitrary (m a), Arbitrary (m (m a)), Arbitrary (m (m (m a))), Show a, Show (m a), Show (m (m a)), Show (m (m (m a))), ExtensionalEquality (m a), ExtensionalEquality (m (m a)), Typeable m, Typeable a)
  => TestTree
unity = testGroup "unity"
  [splitting "left" (join @m) (fmap @m (pure @m @a))
  , splitting "right" join (pure @m @(m a))
  ]
associativity = testProperty "associativity" do join ∘ join ↔ join ∘ fmap (join @m @a)
monadicity = testGroup (unwords ["monadicity", (Prelude.show ∘ typeRep) (Proxy @(m a))]) [unity @m @a, associativity @m @a]

swap ∷ ∀ (n ∷ ★ → ★) (m ∷ ★ → ★) (a ∷ ★) (a' ∷ ★). (Typeable m, Typeable n, Typeable a, CoArbitrary a,
       ExtensionalEquality (m (n a)), ExtensionalEquality (m (n a')), Show (n (m a)), Show (m a), Show (a -> a'),
       Arbitrary a, Arbitrary (m (n a)), Traversable n, Monad m, Monad n, Show (n a), Arbitrary (n a), Arbitrary (m a), Arbitrary a', Show (m (n a)), Arbitrary (n (m a)), Show (n (m (n (m a)))), Arbitrary (n (m (n (m a))))) ⇒ TestTree
swap = testGroup (unwords ["swap laws", (Prelude.show ∘ typeRep) (Proxy @(n a)), (Prelude.show ∘ typeRep) (Proxy @(m a))])
  [ testProperty "S1" \ f → do sequence @n @m @a' ∘ (fmap @n ∘ fmap @m) f ↔ (fmap @m ∘ fmap @n) f ∘ sequence @n @m @a
  , testProperty "S2" do sequence @n @m ∘ pure @n @(m a) ↔ fmap @m (pure @n)
  , testProperty "SS" do sequence @n @m ∘ fmap @n pure ↔ pure @m @(n a)
  , testProperty "s4" do prod ∘ fmap @n @(m (n (m a))) dorp ↔ ((dorp ∷ m (n (m a)) → m (n a)) ∘ (prod @m @n @(m a) ∷ n (m (n (m a))) → m (n (m a))) ∷ n (m (n (m a))) → m (n a))
  ]

prod∷ ∀m n x. (Monad n, Traversable n, Monad m) => n (m (n x)) → m (n x)
prod = fmap @m (join @n) ∘ sequence @n @m
dorp ∷  ∀ m n x. (Monad m, Traversable n) => m (n (m x)) → m (n x)
dorp = join @m ∘ fmap @m (sequence @n @m)

instance (Enum finite, Bounded finite, Eq target) ⇒ Eq ((→) finite target) where (==) f f' = and (zipWith (≡) (fmap f reckon) (fmap f' reckon))
instance (Enum finite, Bounded finite) ⇒ Eq1 ((→) finite) where liftEq (~) f f' = and (zipWith (~) (fmap f reckon) (fmap f' reckon))
instance (Enum finite, Bounded finite) ⇒ Show1 ((→) finite) where liftShowsPrec _ someShowsList _ f = someShowsList (fmap f reckon)

instance Semigroup Bool where (<>) = (&&)
instance Monoid Bool where mempty = True

instance Monad (Δ ( )) where
  Δ {δ = ( )} >>= _ = Δ {δ = ( )}

data Thud x = Thud deriving (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)
instance Show1 Thud where liftShowsPrec _ _ _ _ _ = Prelude.show Thud
instance Eq1 Thud where liftEq _ _ _ = True
instance Applicative Thud where
  pure _ = Thud
  _ <*> Thud = Thud
instance Monad Thud where _ >>= _ = Thud
instance Arbitrary (Thud x) where arbitrary = pure Thud
instance Arbitrary1 Thud where liftArbitrary _ = pure Thud

instance (Monad m, Traversable t, Monad t) ⇒ Monad (m ∘ t) where x >>= y = (W ∘ fmap join ∘ join ∘ fmap sequence ∘ w ∘ fmap w) (fmap y x)
