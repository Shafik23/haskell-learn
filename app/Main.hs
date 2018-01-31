-- {-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Data.Either
import Data.Bool
import Data.Time
import Data.Maybe
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



data Person' = Person' { firstName :: String
                     , lastName :: String
                     , age' :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)

data Expression = Number Int
                | Add' Expression Expression
                | Subtract Expression Expression
                deriving (Eq, Ord, Show)

calculate :: Expression -> Int
calculate (Number x) = x
calculate (Add' x y) = (calculate x) + (calculate y)
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
myWords s = takeWhile notspace s : myWords (dropWhile notspace s)
  where notspace :: Char -> Bool
        notspace = (/= ' ')


acro :: [Char] -> [Char]
acro str = [c | c <-str, elem c ['A'..'Z']]


filterMul3 :: [Integer] -> [Integer]
filterMul3 = filter (\x -> (x `rem` 3) == 0)

howManyMul3 :: [Integer] -> Int
howManyMul3 = length . filterMul3

removeArticles :: [Char] -> [[Char]]
removeArticles = filter (not . (`elem` ["the", "a", "an"])) . words


zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (a:as) (b:bs) = (a, b) : zip as bs

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f _ [] = []
zipWith' f [] _ = []
zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs

zip'' :: [a] -> [b] -> [(a, b)]
zip'' = zipWith' (,)


myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]


data DatabaseItem = DbString String | DbNumber Integer | DbDate UTCTime deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr fetchTimes []
  where
    fetchTimes :: DatabaseItem -> [UTCTime] -> [UTCTime]
    fetchTimes (DbDate time) dates = time : dates
    fetchTimes _ dates = dates

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr fetchDbNumbers []
  where
    fetchDbNumbers :: DatabaseItem -> [Integer] -> [Integer]
    fetchDbNumbers (DbNumber i) dbns = i : dbns
    fetchDbNumbers _ dbns = dbns

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb db = (fromIntegral (sumDb db)) / (fromIntegral (length (filterDbNumber db)))


factorials :: Int -> [Integer]
factorials n = take n $ scanl (*) 1 [1..]


stops = "pbtdkg"
vowels = "aeiou"

tups :: [Char] -> [Char] -> [[Char]]
tups ss vs = [a : b : c : [] | a <- ss, b <- vs, c <- ss, a == 'p']


myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f list = myOr $ map f list

myElem :: Eq a => a -> [a] -> Bool
myElem e = myAny (== e)

myReverse' :: [a] -> [a]
myReverse' = foldl (flip (:)) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x list -> if (f x) then x:list else list) []

squish :: [[a]] -> [a]
squish = foldr f []
  where
    f sublist list = sublist ++ list



data Price = Price Integer deriving (Eq, Show)
data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)
data Vehicle = Car Manufacturer Price | Plane Airline Integer deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

isCar :: Vehicle -> Bool
isCar (Car m p) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane a s) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m p) = m


class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int
  where
    tooMany n = n > 42

newtype Goats = Goats Int deriving Show

instance TooMany Goats
  where
    tooMany (Goats n) = tooMany n


instance TooMany (Int, String)
  where
    tooMany (i, str) = i > 60

instance TooMany (Int, Int)
  where
    tooMany (i, j) = tooMany (i+j)

instance (Num a, TooMany a, Ord a) => TooMany (a, a)
  where
    tooMany (x, y) = x + y > 55


data Person =
  Person { name :: String
         , age :: Int }
         deriving (Eq, Show)


data OperatingSystem =
    GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang =
      Haskell
    | Agda
    | Idris
    | PureScript
    deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgLang }
  deriving (Eq, Show)


allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [ Programmer {os = o, lang = p} | o <- allOperatingSystems, p <- allLanguages ]


data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

testTree :: BinaryTree Integer
testTree =
  Node (Node Leaf 1 Leaf)
  2
  (Node Leaf 3 Leaf)


insertTree :: Ord a => a -> BinaryTree a -> BinaryTree a
insertTree val Leaf = Node Leaf val Leaf
insertTree val (Node left x right)
  | val == x = Node left x right
  | val < x  = Node (insertTree val left) x right
  | val > x  = Node left x (insertTree val right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left x right) =
  Node (mapTree f left) (f x) (mapTree f right)


inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left x right) = (inorder left) ++ [x] ++ (inorder right)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left x right) = [x] ++ (preorder left) ++ (preorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left x right) = (postorder left) ++ (postorder right) ++ [x]

foldTree :: (a -> b -> b)
         -> b
         -> BinaryTree a
         -> b
foldTree f x tree = foldr f x (inorder tree)


isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf search@(x:xs) (y:ys)
  | (x == y)  = isSubseqOf xs ys
  | otherwise = isSubseqOf search ys


capitalizeWords :: String -> [(String, String)]
capitalizeWords s = zip (words s) (map cap (words s))
  where
    cap (c:cs) = (toUpper c) : cs


type Digit = Char
type PhoneValues = String
type Presses = Int

data DaPhone = DaPhone [(Digit, PhoneValues)] deriving (Eq, Show)

daphone = DaPhone
  [('1', "1"),
   ('2', "abc2"),
   ('3', "def3"),
   ('4', "ghi4"),
   ('5', "jkl5"),
   ('6', "mno6"),
   ('7', "pqrs7"),
   ('8', "tuv8"),
   ('9', "wxyz9"),
   ('0', " +_0"),
   ('*', "*^"),
   ('#', "#.,")]

convo :: [String]
convo =
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u ever tasted alcohol lol",
   "Lol ya",
   "Wow ur cool haha. Ur turn",
   "Ok. Do u think I am pretty Lol",
   "Lol ya",
   "Haha thanks just making sure rofl ur turn"]

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone phone) s =
  capital ++ [(fst button, fromJust (elemIndex c (snd button)) + 1)]
  where
    c = toLower s
    button = (filter (\(d, chars) -> elem c chars) phone) !! 0
    capital = bool [] [('*', 1)] (isUpper s)


data Expr = Lit Integer | Add Expr Expr

eval :: Expr -> Integer
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)

printExpr :: Expr -> String
printExpr (Lit n) = show n
printExpr (Add e1 e2) = (printExpr e1) ++ " + " ++ (printExpr e2)


notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s = Just s

replaceThe :: String -> String
replaceThe = unwords . filter (/= "") . map (fromMaybe "" . notThe) . words


countTheBeforeVowel :: String -> Integer
countTheBeforeVowel str = sum (map detect pairs)
  where
    ws = map notThe (words str)
    pairs = zip ws (tail ws)
    detect :: (Maybe String, Maybe String) -> Integer
    detect (Just _, _) = 0
    detect (Nothing, Just (c:cs)) = bool 0 1 (elem c vowels)

countVowels :: String -> Integer
countVowels = sum . map ((\x -> if x then 1 else 0) . (`elem` vowels) . toLower)


newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s = case countVowels s > fromIntegral (length s `div` 2) of
             True -> Nothing
             False -> Just (Word' s)


data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ nat) = 1 + natToInteger nat


isJust' :: Maybe a -> Bool
isJust' (Just _) = True
isJust' _ = False

isNothing' :: Maybe a -> Bool
isNothing' Nothing = True
isNothing' _ = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x _ Nothing = x
mayybee x f (Just j) = f j


lefts' :: [Either a b] -> [a]
lefts' ((Left x):es) = x : lefts' es
lefts' (_:es) = lefts' es
lefts' [] = []

lefts :: [Either a b] -> [a]
lefts xs = foldr f [] xs
  where f (Left x) list = x : list
        f _ list = list

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b))
          -> b
          -> [a]
myUnfoldr f x = case f x of
                  (Just (x, y)) -> x : myUnfoldr f y
                  Nothing -> []
