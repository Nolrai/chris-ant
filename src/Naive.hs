{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Example of a library file. It is also used for testing the test suites.
module Naive
  ( -- * Exported functions
    loop,
    IsOpen,
    Door (..),
    AntEnv (..),
    AntState (..),
    doorSpecs,
    doorStates,
    lineLength,
    antPos,
    src,
    dest,
  )
where

import Control.Lens
import Control.Monad.RWS
import Data.Vector as V
import Data.Vector.Mutable as VM
import Relude

type IsOpen = Bool

data Door = Door {_src :: Word64, _dest :: Word64}
  deriving (Eq, Show)

makeLenses ''Door

data AntEnv = AntEnv {_doorSpecs :: Vector Door, _lineLength :: Word64}
  deriving (Eq, Show)

makeLenses ''AntEnv

data AntState = AntState {_antPos :: Word64, _doorStates :: Vector IsOpen}
  deriving (Eq, Show)

makeLenses ''AntState

takeStep :: RWS AntEnv (Sum Integer) AntState ()
takeStep = do
  tell 1
  antPos %= (+ 1)

checkDoors :: RWS AntEnv (Sum Integer) AntState (Maybe (Int, Door))
checkDoors = do
  doors <- asks (^. doorSpecs)
  here <- gets (^. antPos)
  pure $ V.find (^. (_2 . src) . to (== here)) (V.indexed doors)

getIsOpen :: MonadState AntState m => Int -> m IsOpen
getIsOpen n = doorStates `uses` (! n)

toggleDoor :: MonadState AntState m => Int -> m ()
toggleDoor n = doorStates %= f
  where
    f :: Vector Bool -> Vector Bool
    f = V.modify (\v -> VM.modify v not n)

step :: RWS AntEnv (Sum Integer) AntState ()
step = do
  takeStep
  maybeDoor <- checkDoors
  case maybeDoor of
    Just (n, door) -> do
      isOpen <- getIsOpen n
      when isOpen (antPos .= (door ^. dest))
      toggleDoor n
    Nothing -> pure ()

loop :: RWS AntEnv (Sum Integer) AntState ()
loop = do
  currentPos <- use antPos
  end <- view lineLength
  if currentPos < end
    then step >> loop
    else tell 1