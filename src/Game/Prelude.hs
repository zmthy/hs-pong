------------------------------------------------------------------------------
-- | A better prelude.
module Game.Prelude
    (
      -- * New functions
      (/!)
    , map
    , (++)
    , traceM
    , traceShowM

      -- * New modules
    , module Control.Applicative
    , module Control.Arrow
    , module Control.Monad
    , module Control.Monad.IO.Class
    , module Control.Monad.State

    , module Data.ByteString
    , module Data.Either
    , module Data.Functor
    , module Data.Maybe
    , module Data.Monoid
    , module Data.Text

    , module Debug.Trace

    , module Prelude

    ) where

------------------------------------------------------------------------------
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State

import Data.ByteString (ByteString)
import Data.Either
import Data.Functor
import Data.Maybe
import Data.Monoid
import Data.Text (Text)

import Debug.Trace

import Prelude hiding (map, (++))


------------------------------------------------------------------------------
-- | Integer division.
(/!) :: Integral a => a -> a -> a
(/!) = div


------------------------------------------------------------------------------
-- | General function map.
map :: Functor f => (a -> b) -> f a -> f b
map = fmap


------------------------------------------------------------------------------
-- | General concatenation.
(++) :: Monoid m => m -> m -> m
(++) = mappend


------------------------------------------------------------------------------
-- | General full concatenation.
concat :: Monoid m => [m] -> m
concat = mconcat


------------------------------------------------------------------------------
-- | Monadic trace.
traceM :: Monad m => String -> m ()
traceM s = trace s $ return ()


------------------------------------------------------------------------------
-- | Monadic traceShow.
traceShowM :: (Show a, Monad m) => a -> m ()
traceShowM a = traceShow a $ return ()
