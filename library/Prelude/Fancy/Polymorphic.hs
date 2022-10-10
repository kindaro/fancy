module Prelude.Fancy.Polymorphic (
  module Prelude.Fancy.Arithmetic,
  module Prelude.Fancy.Polymorphic.Polymorph,
  module Prelude.Fancy.Polymorphic.Bloom,
  module Prelude.Fancy.Polymorphic.Shrivel,
  module Prelude.Fancy.Polymorphic.Warp,
  module Prelude.Fancy.Polymorphic.Burst,
  Weld (..),
  Bristle (..),
  TransposeTuple (..),
) where

import Prelude.Unicode

import Prelude.Fancy.Arithmetic
import Prelude.Fancy.Polymorphic.Bloom (Bloom (..), Blooming (..))
import Prelude.Fancy.Polymorphic.Burst (Burst (..), Bursting (..))
import Prelude.Fancy.Polymorphic.Polymorph (Polymorph (..), Polymorphic (..))
import Prelude.Fancy.Polymorphic.Shrivel (Shrivel (..), Shrivelling (..))
import Prelude.Fancy.Polymorphic.Warp (Warp (..), Warping (..))

{- | Compose nested functors everywhere in a tuple list. This lets you then
 apply a natural transformation.
-}
class Weld sundered welded | sundered → welded, welded → sundered where weld ∷ sundered → welded

instance Weld () () where weld _ = ()
instance
  ( Weld sundereds weldeds
  ) ⇒
  Weld (functor (gunctor value) × sundereds) ((functor ∘ gunctor) value × weldeds)
  where
  weld (sundered :× sundereds) = W sundered :× weld sundereds

-- | Apply a tuple list of functions to a matching tuple list of values.
class
  Bristle arrows sources targets
    | arrows → sources
    , arrows → targets
    , sources targets → arrows
  where
  bristle ∷ arrows → sources → targets

instance Bristle () () () where bristle () () = ()
instance Bristle arrows sources targets ⇒ Bristle ((source → target) × arrows) (source × sources) (target × targets) where
  bristle (arrow :× arrows) (source :× sources) = arrow source :× bristle arrows sources

-- | Turn a functor of matching tuple lists into a tuple list of functors.
type family Transposed outer tuple = result where
  Transposed functor () = ()
  Transposed functor (value, transposable) = (functor value, Transposed functor transposable)

class
  transposed ~ Transposed outer tuple ⇒
  TransposeTuple outer tuple transposed
    | outer tuple → transposed
  where
  transposeTuple ∷ outer tuple → transposed
instance TransposeTuple functor () () where transposeTuple _ = ()
instance
  (TransposeTuple functor transposable transposed, Functor functor) ⇒
  TransposeTuple functor (value, transposable) (functor value, transposed)
  where
  transposeTuple functor = (fmap fst functor, (transposeTuple ∘ fmap snd) functor)
