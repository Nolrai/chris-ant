{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example
  ( examples,
    Example (..),
    startState,
    env,
    expected,
    evalExample,
  )
where

import Control.Lens ( (^.), makeLenses )
import Control.Monad.RWS (runRWS, RWS )
import Data.Vector as V ( Vector, map, maximum )
import Naive
    ( Door(Door, _src, _dest), AntEnv(..), src, AntState(..) )
import Relude
    ( ($),
      Eq((==)),
      Integral(mod),
      Num((+)),
      Show,
      Int,
      Integer,
      Maybe(Just),
      Word64,
      Sum(Sum) )

data Example = Example {_startState :: AntState, _env :: AntEnv, _expected :: Maybe Word64}
  deriving (Eq, Show)

makeLenses ''Example

examples :: Vector Example
examples = [example0, example1, example2, example3]

example0 :: Example
example0 =
  Example
    { _startState =
        AntState
          { _antPos = 0,
            _doorStates = V.map (== (1 :: Int)) [0, 1, 0, 1]
          },
      _env =
        let _doorSpecs =
              [ Door {_src = 3, _dest = 2},
                Door {_src = 6, _dest = 5},
                Door {_src = 7, _dest = 4},
                Door {_src = 8, _dest = 1}
              ]
            _lineLength = V.maximum $ V.map (^. src) _doorSpecs
         in AntEnv {..},
      _expected = Just 23
    }

example1 :: Example
example1 =
  Example
    { _startState =
        AntState
          { _antPos = 0,
            _doorStates = V.map (== (1 :: Int)) [1]
          },
      _env =
        let _doorSpecs =
              [Door {_src = 454971987, _dest = 406874902}]
            _lineLength = V.maximum $ V.map (^. src) _doorSpecs
         in AntEnv {..},
      _expected = Just 503069073
    }

example2 :: Example
example2 =
  let _antPos = 0
      _doorStates = V.map (== (1 :: Int)) [0, 0, 0, 0, 0]
      _startState = AntState {..}
      _doorSpecs =
        [ Door {_src = 243385510, _dest = 42245605},
          Door {_src = 644426565, _dest = 574769163},
          Door {_src = 708622105, _dest = 208990040},
          Door {_src = 786625660, _dest = 616437691},
          Door {_src = 899754846, _dest = 382774619}
        ]
      _lineLength = V.maximum $ V.map (^. src) _doorSpecs
      _env = AntEnv {..}
      _expected = Just (_lineLength + 1)
   in Example {..}

example3 :: Example
example3 =
  let _antPos = 0
      _doorStates = V.map (== (1 :: Int)) [1, 0, 0, 1, 0]
      _startState = AntState {..}
      _doorSpecs =
        [ Door {_src = 200000000, _dest = 100000000},
          Door {_src = 600000000, _dest = 400000000},
          Door {_src = 800000000, _dest = 300000000},
          Door {_src = 900000000, _dest = 700000000},
          Door {_src = 1000000000, _dest = 500000000}
        ]
      _lineLength = V.maximum $ V.map (^. src) _doorSpecs
      _env = AntEnv {..}
      _expected = Just 3511295
   in Example {..}

-- evalExample :: Example -> Integer
evalExample :: RWS AntEnv (Sum Integer) AntState a2 -> Example -> Integer
evalExample loop example =
  case runRWS loop (example ^. env) (example ^. startState) of
    (_, _, Sum w) -> w `mod` 998244353