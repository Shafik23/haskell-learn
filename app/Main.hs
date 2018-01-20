-- {-# OPTIONS_GHC -Wall #-}

module Main where

import Data.List
import Data.Char
import System.IO
import Lib ()

-- This is a comment.
-- So is this.

-- Program Entry
main :: IO ()
main = do
  putStrLn "What's your name?"
  name <- getLine
  putStrLn ("hello " ++ name)


doubleMe x = x + x

doubleSmallNumber x = if x > 100
                         then x
                         else x*2


addMe :: Int -> Int -> Int
addMe x y = x + y


factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial x = factorial (x-1) * x


initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname


max' :: (Ord a) => [a] -> a
max' [] = error "Max of empty list doesn't make sense"
max' [x] = x
max' (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = max' xs


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
  let smallerSide = quicksort [a | a <- xs, a <= x]
      biggerSide  = quicksort [a | a <- xs, a > x]
  in smallerSide ++ [x] ++ biggerSide


mergesort :: (Ord a) => [a] -> [a]
mergesort [x] = [x]
mergesort xs = merge (mergesort left) (mergesort right)
  where half = div (length xs) 2
        left = take half xs
        right = drop half xs
        merge [] ys = ys
        merge xs [] = xs
        merge (x:xs) (y:ys)
          | x < y = x:merge xs (y:ys)
          | otherwise = y:merge (x:xs) ys



data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)

data Expression = Number Int
                | Add Expression Expression
                | Subtract Expression Expression
                deriving (Eq, Ord, Show)

calculate :: Expression -> Int
calculate (Number x) = x
calculate (Add x y) = (calculate x) + (calculate y)
calculate (Subtract x y) = (calculate x) - (calculate y)


newHead [] = error "Must give me a non-empty list!"
newHead [a] = a
newHead (x:xs) = x


type Bit = Int

bin2int :: [Bit] -> Int
-- bin2int bits = sum [w*b | (w,b) <- zip weights bits] where weights = iterate (*2) 1 
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id



votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)


-- ------------
-- Haskell Book
-- ------------
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = take (length x `div` 2) x == take (length x `div` 2) (reverse x)

myAbs :: Integer -> Integer
-- myAbs x = if x < 0 then -x else x
myAbs x
  | x < 0 = (-x)
  | otherwise = x


data TisAnInteger = TisAn Integer
instance Eq TisAnInteger where
  (==) (TisAn x) (TisAn y) = x==y

data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
  (==) (Two x y) (Two p q) = (x,y) == (p,q)

data StringOrInt = TisAnInt Int | TisAString String
instance Eq StringOrInt where
  (==) (TisAnInt x) (TisAnInt y) = x==y
  (==) (TisAString x) (TisAString y) = x==y

data DayOfWeek = 
  Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Eq, Show)
instance Ord DayOfWeek where
  compare Fri Fri = EQ
  compare Fri _ = GT
  compare _ Fri = LT
  compare _ _ = EQ


type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)
s1 = Sentence "dogs" "drool"
s2 = Sentence "julie" "loves" "dogs"


chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f x y = (f x) == y

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f i x = (f x) + (fromInteger i)

addOneIfOdd :: Integral a => a -> a
addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = (\n -> n + 1)

functionC :: Ord a => a -> a -> a
functionC x y = case (x > y) of
                  True -> x
                  _ -> y


data WherePenguinsLive = 
  Galapagos
  | Antartica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

data Penguin = Peng WherePenguinsLive deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _ = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng x) = x


tensDigit :: Integral a => a -> a
tensDigit = snd . dm10 . fst . dm10
  where dm10 = (`divMod` 10)

hundredsDigit :: Integral a => a -> a
hundredsDigit = tensDigit . (`div` 10)

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

sum' :: (Eq a, Num a) => a -> a
sum' 0 = 0
sum' n = n + sum' (n-1)

myWords :: [Char] -> [[Char]]
myWords "" = []
myWords (' ':xs) = myWords $ dropWhile (== ' ') xs
myWords s = takeWhile (/= ' ') s : myWords (dropWhile (/= ' ') s)
