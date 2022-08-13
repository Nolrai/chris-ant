{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.Tasty
import Test.Tasty.HUnit
-- import Test.Tasty.SmallCheck

-- import Lib 
import Example
import ParseInput

import Text.Megaparsec
import Data.Vector
import Relude hiding (readFile)
import Data.ByteString (readFile)

main :: IO ()
main = defaultMain $ testGroup "all-tests" tests

tests :: [TestTree]
tests =
  [ testGroup "SmallCheck" scTests
  , testGroup "Unit tests" huTests
  ]

scTests :: [TestTree]
scTests =
  [ 
    -- testProperty "inc == succ" prop_succ
  -- , testProperty "inc . negate == negate . pred" prop_pred
  ]

huTests :: [TestTree]
huTests = case_read_example <$> [0 .. 3]

case_read_example :: Int -> TestTree
case_read_example n = testCase ("read example " <> show n) (readExample n)

readExample :: Int -> Assertion
readExample n = do
  let filepath = "./input/example" <> show n <> ".txt"
  either 
    (assertFailure . errorBundlePretty) -- parse failed
    ((examples ! n) @=?) -- parse succeded, test if it equals expected
    =<< readInputFile filepath

readInputFile :: String -> IO (Either (ParseErrorBundle Text CustomError) Example)
readInputFile filename = do
  (text :: Text) <- decodeUtf8 <$> readFile filename
  pure $ parse parseExample filename text