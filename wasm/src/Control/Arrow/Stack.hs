{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.Stack where

import Prelude hiding (id, fail)

import Control.Arrow
import Control.Arrow.Fail
import Text.Printf
import Data.String
import Data.Profunctor
import GHC.Exts(Constraint)

class (Arrow c, Profunctor c) => ArrowStack val c | c -> val where
    type family Join y (c :: * -> * -> *) :: Constraint

    -- | val in the second argument can be used as an 'expected' value for popping
    pop :: Join y c => c val y -> c val y -> c val y  

    push :: c val ()

pop' :: (Show val, Join val c, IsString e, ArrowFail e c, ArrowStack val c) => c val val
pop' = proc expect ->
    pop success failure -< expect
    where
        success = (proc val -> returnA -< val)
        failure = (proc expect -> fail -< fromString $ printf "Expected value %s on stack" (show expect))
  