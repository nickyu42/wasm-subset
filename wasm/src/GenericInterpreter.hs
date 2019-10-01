{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module GenericInterpreter where

import Control.Arrow

class Arrow c => IsVal v c | c -> v where
    int :: c Int v
    float :: c Int v
    eq :: c (v, v) v
    lt :: c (v, v) v
    add :: c (v, v) v


