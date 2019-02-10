module TimeSeries where

import Data.List
import qualified Data.Map as Map
import Data.Semigroup
import Data.Maybe


file1 :: [(Int, Double)]
file1 = [(1, 200.1), (2, 199.5), (3, 199.4)
        ,(4, 198.8), (5, 199.0), (6, 200.2)
        ,(9, 200.3), (10, 201.2), (12, 202.9)]

file2 :: [(Int, Double)]
file2 = [(11, 200.1), (12, 201.5), (3, 199.4)
        ,(14, 198.8), (15, 203.5), (6, 200.2)
        ,(18, 200.3), (20, 208.8)]

file3 :: [(Int, Double)]
file3 = [(10, 200.1), (11, 201.6), (3, 199.4)
        ,(13, 198.8), (14, 203.5), (6, 200.2)
        ,(24, 200.3), (25, 218.7)]

file4 :: [(Int, Double)]
file4 = [(26, 200.1), (2, 199.5), (3, 199.4)
        ,(29, 198.8), (5, 199.0), (6, 200.2)
        ,(32, 200.3), (10, 201.2), (12, 202.9)
        ,(35, 220.1), (36, 220.6)]


data TS a = TS [Int] [Maybe a]

createTS :: [Int] -> [a] -> TS a
createTS times values = TS completeTimes extendedValues
    where completeTimes = [minimum times .. maximum times]
          timeValueMap = Map.fromList (zip times values)
          extendedValues = map (\v -> Map.lookup v timeValueMap) completeTimes

fileToTS :: [(Int,a)] -> TS a
fileToTS tvPairs = createTS times values
    where (times, values) = unzip tvPairs

showTVPair :: Show a => Int -> (Maybe a) -> String
showTVPair time (Just value) = mconcat [show time, "|", show value, "\n"]
showTVPair time Nothing = mconcat [show time, "|NA\n"]

instance Show a => Show (TS a) where
    show (TS times values) = mconcat rows
        where rows = zipWith showTVPair times values
