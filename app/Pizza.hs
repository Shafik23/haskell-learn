module Pizza where

import Data.List
import qualified Data.Map as Map
import Data.Semigroup
import Data.Maybe


type Pizza = (Double, Double)

areaGivenDiameter :: Double -> Double
areaGivenDiameter d = (d^2)/2 * pi

costPerInch :: Pizza -> Double
-- costPerInch (size, cost) = cost / areaGivenDiameter size
costPerInch (size, cost) = (/) cost $ areaGivenDiameter size

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 = if costP1 < costP2
                         then p1
                         else p2
                         where costP1 = costPerInch p1
                               costP2 = costPerInch p2

describePizza :: Pizza -> String
describePizza (size, cost) = "The " ++ show size ++ " pizza " ++
    "is cheaper at " ++ show costSqInch ++ " per square inch"
        where costSqInch = costPerInch (size, cost)


main :: IO ()
main = do
    putStrLn "What is the size of pizza 1?"
    size1 <- getLine
    putStrLn "What is the cost of pizza 1?"
    cost1 <- getLine
    putStrLn "What is the size of pizza 2?"
    size2 <- getLine
    putStrLn "What is the ost of pizza 2?"
    cost2 <- getLine
    let p1 = (read size1, read cost1)
    let p2 = (read size2, read cost2)
    let betterPizza = comparePizzas p1 p2
    putStrLn (describePizza betterPizza)


-- This is broken in GHCi due to ... reasons, but should
-- work in the cli.
reverser :: IO ()
reverser = do
    input <- getContents
    let reversed = reverse input
    putStrLn reversed
