{- |

Op provides operators for writing easier-to-read Haskell. It provides new
operators with a consistent "look and feel" including fixity direction and
precedence, resulting in easier- and quicker-to-read code especially when used
on long chains of expressions.

All right-facing operators are defined with @infixl 1@ which is the same as
@>>=@, so you can chain all of these together without using parentheses.

All left-facing operators are defined with @infixr 1@ which is the same as
@=<<@, so you can chain all of these together also without using parentheses.

Unlike <https://github.com/tfausak/flow Flow> and
<https://github.com/ombocomp/FunctorMonadic FunctorMonadic> we do not restrict
ourselves to functions and functors respectively, but we try to cover as many
operators as possible.

This means we do conflict with a few non-Prelude base operators (@>>>@, @>=>@),
but that is the trade-off we chose. They are used less commonly than the ones
we chose to retain compatibility with, IOO their inconsistency is part of the
reason why they are used less commonly, and this package is trying to fix that.

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

-}
module Control.Op where

import qualified Control.Category as C
import qualified Control.Monad    as M
import qualified Data.Function    as F
import qualified Data.Functor     as F

-- | LTR function application.
(|>) :: a -> (a -> b) -> b
(|>) = (F.&)
infixl 1 |>
{-# INLINE (|>) #-}

-- | RTL function application.
(<|) :: (a -> b) -> a -> b
(<|) = ($)
infixr 1 <|
{-# INLINE (<|) #-}

-- | LTR function composition.
(.>) :: (a -> b) -> (b -> c) -> (a -> c)
(.>) = (C.>>>)
infixl 1 .>
{-# INLINE (.>) #-}

-- | RTL function composition.
(<.) :: (b -> c) -> (a -> b) -> (a -> c)
(<.) = (.)
infixr 1 <.
{-# INLINE (<.) #-}

-- | LTR category composition.
(>>>) :: C.Category f => (f a b) -> (f b c) -> (f a c)
(>>>) = (C.>>>)
infixl 1 >>>
{-# INLINE (>>>) #-}

-- | RTL category composition.
(<<<) :: C.Category f => (f b c) -> (f a b) -> (f a c)
(<<<) = (C.<<<)
infixr 1 <<<
{-# INLINE (<<<) #-}

-- | LTR functor application.
(>$>) :: Functor f => f a -> (a -> b) -> f b
(>$>) = (F.<&>)
infixl 1 >$>
{-# INLINE (>$>) #-}

-- | RTL functor application.
(<$<) :: Functor f => (a -> b) -> f a -> f b
(<$<) = (<$>)
infixr 1 <$<
{-# INLINE (<$<) #-}

-- Prelude.>>= is already infixl 1
-- Prelude.=<< is already infixr 1

-- | LTR monad composition.
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
(>=>) = (M.>=>)
infixl 1 >=>
{-# INLINE (>=>) #-}

-- Prelude.>=> is already infixl 1
