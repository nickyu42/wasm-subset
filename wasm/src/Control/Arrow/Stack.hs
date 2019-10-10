{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.Stack where

import Prelude hiding (id, fail)

import Control.Arrow
import Data.Profunctor

-- | Typeclass for stack computations
class (Arrow c, Profunctor c) => ArrowStack v c | c -> v where
    -- the input argument for pop can be used as an 'expected' value
    pop :: c v v
    peek :: c () v
    push :: c v ()