import Lib (inc)
import ParseInput
import Example

main :: IO ()
main = print . inc $ (41 :: Int)
