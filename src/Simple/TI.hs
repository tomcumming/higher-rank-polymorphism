module Simple.TI where

import Control.Monad.State
import Simple.Type (Ext)

type TI = State Ext

runTI :: TI a -> a
runTI x = fst $ runState x 100

fresh :: TI Ext
fresh = do
  x <- get
  put (succ x)
  return x
