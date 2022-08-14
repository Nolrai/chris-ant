{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Example
import Naive
import ParseInput
import Text.Megaparsec
import Relude

main :: IO ()
main = do
    [filePath] <- getArgs
    input <- readInputFile filePath
    print input
    putStrLn "proccessing"
    print $ evalExample Naive.loop input

readInputFile :: String -> IO Example
readInputFile filename = do
  (text :: Text) <- decodeUtf8 <$> readFileBS filename
  case parse parseExample filename text of
    Left err -> error . toText $ errorBundlePretty err
    Right example -> pure example