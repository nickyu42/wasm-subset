{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.Stack where

import Prelude hiding (id, fail)

import Control.Arrow
-- import Control.Arrow.Fail
-- import Text.Printf
-- import Data.String
import Data.Profunctor
-- import GHC.Exts(Constraint)

-- | Typeclass for stack computations
class (Arrow c, Profunctor c) => ArrowStack v c | c -> v where
    -- type family Join y (c :: * -> * -> *) :: Constraint

    -- val in the second argument can be used as an 'expected' value for popping
    -- pop :: Join y c => c val y -> c val y -> c val y

    -- the input argument for pop can be used as an 'expected' value
    pop :: c v v
    peek :: c () v
    push :: c v ()

-- pop' :: (Show val, Join val c, IsString e, ArrowFail e c, ArrowStack val c) => c val val
-- pop' = proc expect ->
--     pop success failure -< expect
--     where
--         success = (proc val -> returnA -< val)
--         failure = (proc expect -> fail -< fromString $ printf "Expected value %s on stack" (show expect))
  