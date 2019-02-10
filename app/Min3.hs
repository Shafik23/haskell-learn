module Min3 where

import Data.List
import qualified Data.Map as Map
import Data.Semigroup
import Data.Maybe


minOfThree :: (Ord a) => a -> a -> a -> a
minOfThree v1 v2 v3 = min v1 (min v2 v3)

readInt :: IO Int
readInt = read <$> getLine

minOfInts :: IO Int
minOfInts = minOfThree <$> readInt <*> readInt <*> readInt

main :: IO ()
main = do
    putStrLn "Enter three numbers"
    minInt <- minOfInts
    putStrLn (show minInt ++ " is the smallest")
