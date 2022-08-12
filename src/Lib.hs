import Control.Monad.RWS

-- | Example of a library file. It is also used for testing the test suites.
module Lib
  (
    -- * Exported functions
    inc
  ) where

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

data Door = Door {src :: Int64, dest :: Int64}

data AntWorld = {doorSpecs :: Vector Door, lineLength :: Int}

data AntState = AntState {antPos :: Integer, time :: Integer, doorStates :: Vector IsOpen}

naiveStep :: RWS AntWorld (Sum Integer) AntState
naiveStep = do
  takeStep
  maybeDoor <- checkDoors
  match maybeDoor with
    Just (ix, dest) -> do
      isOpen <- (! ix) <$> gets doorStates
      when isOpen (setAntPos dest)
      toggleDoor ix

