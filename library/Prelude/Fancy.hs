module Prelude.Fancy
  ( module Prelude
  , module Prelude.Unicode
  , module Data.Monoid.Unicode
  , module Control.Applicative.Unicode
  , module Prelude.Fancy.Assorti
  , module Prelude.Fancy.Arithmetic
  , module Prelude.Fancy.Curry
  , module Prelude.Fancy.Fork
  , module Prelude.Fancy.Polymorphic
  , module Prelude.Fancy.Converge
  , module Data.Text
  , module GHC.Generics
  , module Control.Monad
  , module Data.Function
  , module Data.Foldable
  ) where

import Prelude.Unicode
import Prelude hiding (read, show, (.), print, head, tail, init, last, fst, snd)
import Data.Foldable (toList, traverse_)

import Data.Monoid.Unicode ((⊕))
import Control.Applicative.Unicode ((⊛))

import Prelude.Fancy.Arithmetic
import Prelude.Fancy.Assorti
import Prelude.Fancy.Converge
import Prelude.Fancy.Curry
import Prelude.Fancy.Fork
import Prelude.Fancy.Polymorphic

import Data.Text (Text)
import GHC.Generics (Generic)
import Control.Monad (join)
import Data.Function
