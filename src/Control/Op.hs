{- |

Op provides operators for writing easier-to-read Haskell. It provides new
operators with a consistent "look and feel" including fixity direction and
precedence, resulting in easier- and quicker-to-read code especially when used
on long chains of expressions.

All right-facing operators are defined with @infixl 1@ which is the same as
'Control.Monad.>>=', so you can chain all of these together without using
parentheses.

All left-facing operators are defined with @infixr 1@ which is the same as
'Control.Monad.=<<', so you can chain all of these together also without using
parentheses.

Unlike <https://github.com/tfausak/flow Flow> and
<https://github.com/ombocomp/FunctorMonadic FunctorMonadic> we do not restrict
ourselves to functions and functors respectively, but we try to cover as many
operators as possible.

This means we conflict with some non-Prelude base operators (search "redefined"
below), but that is the trade-off we chose. They are used less commonly than
the ones we retain compatibility with, IOO their inconsistency is part of the
reason why they are used less commonly, and this package tries to fix that.

== Examples

>>> :set -XTupleSections
>>> import Control.Op
>>> import Data.Functor
>>> import qualified Data.Map.Strict as M
>>> :{
data InnerMap k v = InnerMap
  { innerMeta :: !()
  , innerMap :: !(M.Map k v)
  }
:}

>>> type MultiMap k v = M.Map Integer (InnerMap k v)

Old way, needs extra parens due to @\<$\>@'s fixity:

>>> :{
lookupOldR :: Ord k => MultiMap k v -> (Integer, k) -> Maybe (Integer, v)
lookupOldR m (i, k) = (i,) <$> (M.lookup k . innerMap =<< M.lookup i m)
:}

or, slightly better but the @. innerMap@ still breaks up the LTR flow:

>>> :{
lookupOldL :: Ord k => MultiMap k v -> (Integer, k) -> Maybe (Integer, v)
lookupOldL m (i, k) = M.lookup i m >>= M.lookup k . innerMap <&> (i,)
:}

New way:

>>> :{
lookupNewR :: Ord k => MultiMap k v -> (Integer, k) -> Maybe (Integer, v)
lookupNewR m (i, k) = (i,) <$< M.lookup k =<< innerMap <$< M.lookup i <| m
:}

>>> :{
lookupNewL :: Ord k => MultiMap k v -> (Integer, k) -> Maybe (Integer, v)
lookupNewL m (i, k) = m |> M.lookup i >$> innerMap >>= M.lookup k >$> (i,)
:}

== Applicative

We omit defining an equivalent of 'Control.Applicative.<*>' because it does not
fit into our system very well. The main use-case for 'Control.Applicative.<*>'
translated into our system would look something like:

@
  (((f <$< a) <*< b) <*< c)
@

which is worse from a readability perspective, compared to the standard form:

@
  f \<$\> a \<*\> b \<*\> c
@

We could define extra "flipped" operators like:

@
  f >&> a >\@> b >\@> c
@

with @(>&>) = (<$<)@ and @(>\@>) = (<*<)@ with flipped fixities, but didn't see
a major demand to do this ATTOW. If you want this, please file a PR.

-}
module Control.Op
  ( (|>)
  , (<|)
  , (.>)
  , (<.)
  , (>>>)
  , (<<<)
  , (>$>)
  , (<$<)
  , (>$=)
  , (=$<)
  , (>*=)
  , (=*<)
  , (>$>-)
  , (-<$<)
  , (>$>=)
  , (=<$<)
  -- Note: haddock does not support redocumenting reexported symbols, the below
  -- hack is the best we can achieve. It results in documentation detached from
  -- the re-exported symbol entry but is directly below it at least.
  , (>>)
  -- | LTR applicative replacement, constrained to a monad.
  --
  -- This is 'Control.Monad.>>'.
  , (<<)
  , (>>=)
  -- | LTR monad application.
  --
  -- This is 'Control.Monad.>>='.
  , (=<<)
  -- | RTL monad application.
  --
  -- This is 'Control.Monad.=<<'.
  , (>=>)
  , (<=<)
  , (^>>)
  , (<<^)
  , (>>^)
  , (^<<)
  )
where

import qualified Control.Applicative as A
import           Control.Arrow       (Arrow (..))
import qualified Control.Arrow       as Ar
import qualified Control.Category    as C
import qualified Control.Monad       as M
import qualified Data.Foldable       as T
import qualified Data.Function       as F
import qualified Data.Functor        as F
import qualified Data.Traversable    as T

-- | LTR function application.
--
-- Same as 'Data.Function.&' with a consistent fixity.
--
-- Also same as the ocaml function <https://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html#VAL(|%3E) (|\>)>
(|>) :: a -> (a -> b) -> b
(|>) = (F.&)
infixl 1 |>
{-# INLINE (|>) #-}

-- | RTL function application.
--
-- Same as 'GHC.Base.$' with a consistent fixity.
(<|) :: (a -> b) -> a -> b
(<|) = ($)
infixr 1 <|
{-# INLINE (<|) #-}

-- | LTR function composition.
--
-- Same as 'GHC.Base.flip' 'GHC.Base..' with a consistent fixity.
(.>) :: (a -> b) -> (b -> c) -> (a -> c)
(.>) = flip (.)
infixl 1 .>
{-# INLINE (.>) #-}

-- | RTL function composition.
--
-- Same as 'GHC.Base..' with a consistent fixity.
(<.) :: (b -> c) -> (a -> b) -> (a -> c)
(<.) = (.)
infixr 1 <.
{-# INLINE (<.) #-}

-- | LTR category composition.
--
-- This is 'Control.Category.>>>' but with a redefined consistent fixity.
(>>>) :: C.Category f => f a b -> f b c -> f a c
(>>>) = (C.>>>)
infixl 1 >>>
{-# INLINE (>>>) #-}

-- | RTL category composition.
--
-- This is 'Control.Category.<<<'.
(<<<) :: C.Category f => f b c -> f a b -> f a c
(<<<) = (C.<<<)
infixr 1 <<<
{-# INLINE (<<<) #-}

-- | LTR functor application.
--
-- Same as 'Data.Functor.<&>' with a consistent fixity.
(>$>) :: Functor f => f a -> (a -> b) -> f b
(>$>) = (F.<&>)
infixl 1 >$>
{-# INLINE (>$>) #-}

-- | RTL functor application.
--
-- Same as 'Data.Functor.<$>' with a consistent fixity.
(<$<) :: Functor f => (a -> b) -> f a -> f b
(<$<) = (<$>)
infixr 1 <$<
{-# INLINE (<$<) #-}

-- | LTR functor replacement.
--
-- Same as 'Data.Functor.$>' with a consistent fixity.
(>$=) :: Functor f => f a -> b -> f b
(>$=) = (F.$>)
infixl 1 >$=
{-# INLINE (>$=) #-}

-- | RTL functor replacement.
--
-- Same as 'Data.Functor.<$' with a consistent fixity.
(=$<) :: Functor f => b -> f a -> f b
(=$<) = (<$)
infixr 1 =$<
{-# INLINE (=$<) #-}

-- | LTR applicative replacement.
--
-- Same as 'Control.Applicative.*>' with a consistent fixity.
(>*=) :: Applicative f => f a -> f b -> f b
(>*=) = (A.*>)
infixl 1 >*=
{-# INLINE (>*=) #-}

-- | RTL applicative replacement.
--
-- Same as 'Control.Applicative.<*' with a consistent fixity.
(=*<) :: Applicative f => f b -> f a -> f b
(=*<) = (A.<*)
infixr 1 =*<
{-# INLINE (=*<) #-}

-- | LTR applicative fold.
--
-- Same as 'Data.Foldable.for_' as an operator with a consistent fixity.
(>$>-) :: (Foldable t, Applicative f) => t a -> (a -> f b) -> f ()
(>$>-) = T.for_
infixl 1 >$>-
{-# INLINE (>$>-) #-}

-- | RTL applicative fold.
--
-- Same as 'Data.Foldable.traverse_' as an operator with a consistent fixity.
(-<$<) :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
(-<$<) = T.traverse_
infixr 1 -<$<
{-# INLINE (-<$<) #-}

-- | LTR applicative traversal.
--
-- Same as 'Data.Traversable.for' as an operator with a consistent fixity.
(>$>=) :: (Traversable t, Applicative f) => t a -> (a -> f b) -> f (t b)
(>$>=) = T.for
infixl 1 >$>=
{-# INLINE (>$>=) #-}

-- | RTL applicative traversal.
--
-- Same as 'Data.Traversable.traverse' as an operator with a consistent fixity.
(=<$<) :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
(=<$<) = T.traverse
infixr 1 =<$<
{-# INLINE (=<$<) #-}

-- | RTL applicative replacement, constrained to a monad.
--
-- Surprisingly, this is not defined in the base libraries.
(<<) :: Monad m => m a -> m b -> m a
(<<) = (=*<)
infixr 1 <<
{-# INLINE (<<) #-}

-- | LTR monad composition.
--
-- This is 'Control.Monad.>=>' but with a redefined consistent fixity.
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
(>=>) = (M.>=>)
infixl 1 >=>
{-# INLINE (>=>) #-}

-- | RTL monad composition.
--
-- This is 'Control.Monad.<=<'.
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
(<=<) = (M.<=<)
infixr 1 <=<
{-# INLINE (<=<) #-}

-- | LTR function-arrow composition.
--
-- This is 'Control.Arrow.^>>' but with a redefined consistent fixity.
(^>>) :: Arrow r => (a -> b) -> r b c -> r a c
(^>>) = (Ar.^>>)
infixl 1 ^>>
{-# INLINE (^>>) #-}

-- | RTL function-arrow composition.
--
-- This is 'Control.Arrow.<<^'.
(<<^) :: Arrow r => r b c -> (a -> b) -> r a c
(<<^) = (Ar.<<^)
infixr 1 <<^
{-# INLINE (<<^) #-}

-- | LTR arrow-function composition.
--
-- This is 'Control.Arrow.>>^' but with a redefined consistent fixity.
(>>^) :: Arrow r => r a b -> (b -> c) -> r a c
(>>^) = (Ar.>>^)
infixl 1 >>^
{-# INLINE (>>^) #-}

-- | RTL arrow-function composition.
--
-- This is 'Control.Arrow.^<<'.
(^<<) :: Arrow r => (b -> c) -> r a b -> r a c
(^<<) = (Ar.^<<)
infixr 1 ^<<
{-# INLINE (^<<) #-}
