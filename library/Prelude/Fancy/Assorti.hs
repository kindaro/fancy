module Prelude.Fancy.Assorti where

import Prelude.Unicode
import Prelude hiding (read, show)
import Prelude qualified

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

bind ∷ Monad monad ⇒ (input → monad output) → monad input → monad output
bind = (=<<)

for ∷ Functor functor ⇒ functor α → (α → β) → functor β
for = flip fmap

type ByteArray = ByteArray.ByteString
type ByteStream = ByteStream.ByteString

show ∷ Show showly ⇒ showly → Text
show = Text.pack ∘ Prelude.show

read ∷ Read readly ⇒ Text → Maybe readly
read = Base.readMaybe ∘ Text.unpack

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
