{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Example of a library file. It is also used for testing the test suites.
module Lib
  (
    -- * Exported functions
    inc,
    naiveLoop
  ) where
    
import Relude
import Control.Monad.RWS
import Data.Vector as V
import Control.Lens

-- | Increment one 'Num' value.
--
--  >>> let answer = 42 :: Int
--  >>> let prev = answer - 1
--  >>> inc prev
--  42
--  >>> succ . Prelude.last . Prelude.take prev . iterate inc $ 1
--  42
--
--  Properties:
--
--  prop> succ x == inc x
--  prop> inc (negate x) == negate (pred x)
--
inc :: Num a => a -- ^ value to increment
             -> a -- ^ result
inc x = x + 1

type IsOpen = Bool

data Door = Door {_src :: Word64, _dest :: Word64}
makeLenses ''Door

data AntEnv = AntEnv {_doorSpecs :: Vector Door, _lineLength :: Word64}
makeLenses ''AntEnv

data AntState = AntState {_antPos :: Word64, _doorStates :: Vector IsOpen}
makeLenses ''AntState

takeStep :: RWS AntEnv (Sum Integer) AntState ()
takeStep = do
  tell 1
  antPos %= inc

checkDoors :: RWS AntEnv (Sum Integer) AntState (Maybe Door)
checkDoors = do
  doors <- asks (^. doorSpecs)
  here <- gets (^. antPos)
  doorState <- gets (^. doorStates)
  let openDoors = V.map (^. _1) . V.filter (^. _2) $ V.zip doors doorState
  pure $ V.find (^. src . to (== here)) openDoors

naiveStep :: RWS AntEnv (Sum Integer) AntState ()
naiveStep = do
  takeStep
  traverse_ ((antPos .=) . (^. dest)) =<< checkDoors

naiveLoop :: RWS AntEnv (Sum Integer) AntState ()
naiveLoop = do
  currentPos <- use antPos
  end <- view lineLength
  when (currentPos < end) (naiveStep >> naiveLoop)