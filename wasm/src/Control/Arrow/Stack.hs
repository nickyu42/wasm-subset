{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Arrows #-}
module Control.Arrow.Stack where

import Prelude hiding (id, fail)

import Control.Arrow
import Data.Profunctor

import Syntax(WasmType)

-- | Typeclass for stack computations
class (Arrow c, ArrowChoice c, Profunctor c) => ArrowStack v c | c -> v where
    -- the input argument for pop can be used as an 'expected' value
    pop :: c WasmType v
    peek :: c () v
    push :: c v ()
    getStack :: c () [v]
    putStack :: c [v] ()

    clear :: c () ()
    clear = proc _ -> putStack -< []

    popN :: c [WasmType] [v]
    popN = proc ts -> case ts of
        t:ts' -> do
            v <- pop -< t
            vs <- popN -< ts'
            returnA -< v:vs
        [] -> returnA -< []

    pushN :: c [v] ()
    pushN = proc vs -> do
        s <- getStack -< ()
        putStack -< vs ++ s
        returnA -< ()
        
