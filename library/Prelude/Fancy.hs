module Prelude.Fancy
  ( module Prelude
  , module Prelude.Unicode
  , module Data.Monoid.Unicode
  , module Prelude.Fancy.Assorti
  , module Prelude.Fancy.Arithmetic
  , module Prelude.Fancy.Curry
  , module Prelude.Fancy.Fork
  , module Prelude.Fancy.Polymorphic
  , module Prelude.Fancy.Converge
  , module Data.Text
  , module Control.Monad
  ) where

import Prelude.Unicode
import Prelude hiding (read, show, (.))

import Data.Monoid.Unicode
import Control.Monad (join)

import Prelude.Fancy.Arithmetic
import Prelude.Fancy.Assorti
import Prelude.Fancy.Converge
import Prelude.Fancy.Curry
import Prelude.Fancy.Fork
import Prelude.Fancy.Polymorphic

import Data.Text (Text)
