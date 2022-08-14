{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}
-- {-# LANGUAGE TypedHoles #-}

import Test.Hspec hiding (Example)

-- import Lib 
import Example
import ParseInput

import Text.Megaparsec
import Data.Vector
import Relude hiding (readFile, forM_)
import Data.ByteString (readFile)

main :: IO ()
main = hspec $ do
  describe "ParseInput" $ 
    [0 .. 3] `forM_` case_read_example

case_read_example :: Int -> Spec
case_read_example n = it ("can read example " <> show n) (readExample n)

readExample :: Int -> Expectation
readExample n = do
  let filepath = "./input/example" <> show n <> ".txt"
  either 
    (expectationFailure . errorBundlePretty) -- parse failed
    (`shouldBe` (examples ! n)) -- parse succeded, test if it equals expected
    =<< readInputFile filepath

readInputFile :: String -> IO (Either (ParseErrorBundle Text CustomError) Example)
readInputFile filename = do
  (text :: Text) <- decodeUtf8 <$> readFile filename
  pure $ parse parseExample filename text