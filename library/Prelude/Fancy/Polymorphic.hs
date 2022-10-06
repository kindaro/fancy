module Prelude.Fancy.Polymorphic (
  module Prelude.Fancy.Arithmetic,
  module Prelude.Fancy.Polymorphic.Polymorph,
  module Prelude.Fancy.Polymorphic.Bloom,
  module Prelude.Fancy.Polymorphic.Shrivel,
  module Prelude.Fancy.Polymorphic.Warp,
  module Prelude.Fancy.Polymorphic.Burst,
  Weld (..),
) where

import Prelude.Fancy.Arithmetic
import Prelude.Fancy.Polymorphic.Bloom (Bloom (..), Blooming (..))
import Prelude.Fancy.Polymorphic.Burst (Burst (..), Bursting (..))
import Prelude.Fancy.Polymorphic.Polymorph (Polymorph (..), Polymorphic (..))
import Prelude.Fancy.Polymorphic.Shrivel (Shrivel (..), Shrivelling (..))
import Prelude.Fancy.Polymorphic.Warp (Warp (..), Warping (..))

class Weld sundered welded | sundered → welded, welded → sundered where weld ∷ sundered → welded
instance Weld () () where weld _ = ()
instance
  ( Weld sundereds weldeds
  ) ⇒
  Weld (functor (gunctor value) × sundereds) ((functor ∘ gunctor) value × weldeds)
  where
  weld (sundered :× sundereds) = W sundered :× weld sundereds
