{-# LANGUAGE AllowAmbiguousTypes #-}

module Prelude.Fancy.Assorti where

import Prelude.Unicode
import Prelude hiding (read, show, print)

import Control.Applicative
import Control.Monad.Writer
import Data.ByteString qualified as ByteArray
import Data.ByteString.Lazy qualified as ByteStream
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Error qualified as Text
import Data.Text.Lazy qualified as Texts
import Data.Text.Lazy.Encoding qualified as Texts
import Text.Read qualified as Base
import Text.Show.Pretty qualified as Pretty
import Numeric.Natural
import GHC.TypeLits
import Data.Proxy

bind ∷ Monad monad ⇒ (input → monad output) → monad input → monad output
bind = (=<<)

for ∷ Functor functor ⇒ functor α → (α → β) → functor β
for = flip fmap

type ℕ = Natural

reify ∷ ∀ size number. (KnownNat size, Integral number) ⇒ number
reify = fromIntegral (natVal (Proxy @size))

predecessor ∷ ℕ → Maybe ℕ
predecessor 0 = Nothing
predecessor n = Just (n − 1)

pattern Zero ∷ ℕ
pattern Zero = 0

pattern Successor :: ℕ → ℕ
pattern Successor n ← (predecessor → Just n)
  where Successor n = n + 1

{-# complete Zero, Successor #-}

type ByteArray = ByteArray.ByteString
type ByteStream = ByteStream.ByteString
type UnicodeArray = Text.Text
type UnicodeStream = Texts.Text

show ∷ Show showly ⇒ showly → Text
show = Text.pack ∘ Pretty.ppShow

read ∷ Read readly ⇒ Text → Maybe readly
read = Base.readMaybe ∘ Text.unpack

print ∷ Show showly ⇒ showly → IO ( )
print = Pretty.pPrint

class Utf8 bytes string | bytes → string, string → bytes where utf8 ∷ bytes → string
instance Utf8 ByteArray Text where utf8 = Text.decodeUtf8Lenient
instance Utf8 ByteStream Texts.Text where utf8 = Texts.decodeUtf8With Text.lenientDecode

memptify ∷ Monoid monoid ⇒ Bool → monoid → monoid
memptify check thing = if check then thing else mempty

guarded ∷ Alternative monad ⇒ Bool → α → monad α
guarded check thing = if check then pure thing else empty

constant ∷ α → β → α
constant = const

writ ∷ ([α] → β) → Writer [α] () → β
writ = (∘ (snd ∘ runWriter))

say ∷ α → Writer [α] ()
say = tell ∘ pure

picky ∷ [α] → Maybe α
picky [x] = Just x
picky _ = Nothing

reckon ∷ (Enum α, Bounded α) ⇒ [α]
reckon = [minBound .. maxBound]
