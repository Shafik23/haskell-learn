-- {-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Test.QuickCheck
import Data.Either
import Data.Bool
import Data.Time
import Data.Maybe
import Data.List
import Data.Char
import Data.Monoid
import Data.Functor.Identity as DFI 
import qualified Data.Semigroup as S
import System.IO
import System.Random
import Control.Monad (join, foldM)
import Control.Monad.Writer
import Control.Monad.State
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


max'' :: (Ord a) => [a] -> a
max'' [] = error "Max of empty list doesn't make sense"
max'' [x] = x
max'' (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = max'' xs


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

data TwoIntegers = Two' Integer Integer
instance Eq TwoIntegers where
  (==) (Two' x y) (Two' p q) = (x,y) == (p,q)

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

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f list = myOr $ map f list

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

-- ViewPatterns are cool!
-- In this case, we're saying "if the input *3 == 12, then return 0, otherwise return x+7"
funcky :: Int -> Int
funcky ((*3) -> 12) = 0
funcky x = (x + 7)


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
allProgrammers = [ Programmer {os = o, lang = p} | o <- allOperatingSystems, p <- allLanguages, x <- [1..13]]


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


data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only o1) (Only o2) = Only (mappend o1 o2)
  mappend x Nada = x
  mappend Nada x = x

instance Arbitrary (Optional String)
  where arbitrary = frequency [ (1, return Nada), (1, return (Only "cat")) ]

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a


newtype XFirst' a = 
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)
  
instance Monoid (XFirst' a) where
  mempty = First' Nada
  mappend x@(First' (Only o)) _ = x
  mappend (First' Nada) x = x

data MyBe a = N | P a

-- instance Monoid (MyBe a) where
instance Functor MyBe where
  fmap = undefined



data Trivial = Trivial deriving (Eq, Show)

instance S.Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semiGroupAssoc :: (Eq m, S.Semigroup m) => m -> m -> m -> Bool
semiGroupAssoc a b c = 
  (a S.<> (b S.<> c)) == ((a S.<> b) S.<> c)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool


newtype BoolConj = BoolConj Bool deriving (Eq, Show)
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance S.Semigroup BoolConj where
  _ <> (BoolConj True) = (BoolConj True)
  _ <> (BoolConj False) = (BoolConj False)


newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance S.Semigroup b => S.Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (\x -> f x S.<> g x)

newtype Comp a = Comp { unComp :: (a -> a) }

instance S.Semigroup (Comp a) where
  (Comp x) <> (Comp y) = Comp (x . y)


myisempty :: (Monoid m, Eq m) => m -> Bool
myisempty x = (x == (mempty))

instance Monoid Trivial where
  mempty = Trivial
  mappend = (S.<>)

instance Monoid BoolConj where
  mempty = (BoolConj True)
  mappend = (S.<>)

instance Monoid BoolDisj where
  mempty = (BoolDisj True)
  mappend _ (BoolDisj True) = (BoolDisj True)
  mappend (BoolDisj True) _ = (BoolDisj True)
  mappend _ _ = (BoolDisj False)


newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem (\x -> (mempty, x))
  mappend m1 m2 = Mem (\x -> (fst (mf1 x) <> fst (mf2 x), snd (mf1 (snd (mf2 x)))))
    where mf1 = runMem m1
          mf2 = runMem m2

f' = Mem (\s -> ("hi", s + 1))

main1 = do
  let rmzero = (runMem mempty) 0
      rmleft = (runMem (f' <> mempty)) 0
      rmright = (runMem (mempty <> f')) 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0


a = fmap (+1) $ read "[1]" :: [Int]
b = (fmap.fmap) (++ "lol") (Just ["Hi,", "Hello"])
c = fmap (*2) ((\x -> x - 2))

d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . ("123" ++) . show) ioi
     in fmap (*3) changed


test_657 = do
  print $ a
  print $ b
  print $ c 1
  print $ d 0
  x <- e
  print $ x



newtype Identity a = Identity a
instance Functor Main.Identity where
  fmap f (Main.Identity x) = Main.Identity (f x)

data Pair a = Pair a a
instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

data Two a b = Two a b
instance Functor (Two x) where
  fmap f (Two x y) = Two x (f y)

data Three a b c = Three a b c
instance Functor (Three a b) where 
  fmap f (Three x y z) = Three x y (f z)

data Three' a b = Three' a b b 
instance Functor (Three' a) where 
  fmap f (Three' x y z) = Three' x (f y) (f z)

data Four a b c d = Four a b c d
instance Functor (Four a b c) where
  fmap f (Four x y z w) = Four x y z (f w)

data Four' a b = Four' a a a b
instance Functor (Four' a) where
  fmap f (Four' x y z w) = Four' x y z (f w)

-- Doesn't work due to kinded-ness of "Trivial" being too restrictive
-- instance Functor Trivial where
--   fmap f Trivial = Trivial


data Possibly a = LolNop | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNop = LolNop
  fmap f (Yeppers x) = Yeppers (f x)


data Sum' a b = Frst a | Scond b deriving (Eq, Show)

instance Functor (Sum' a) where
  fmap f (Scond x) = Scond (f x)


data Wrap t1 t2 = Wrap (t1 t2) deriving (Eq, Show)

instance Functor t => Functor (Wrap t) where
  fmap f (Wrap x) = Wrap (fmap f x)



data BoolAndSomethingElse a = False' a | True' a

instance Functor (BoolAndSomethingElse) where
  fmap f (False' a) = False' (f a)
  fmap f (True' a) = True' (f a)


data BoolAndMaybeSomethingElse a = Falsish | Truish a

instance Functor (BoolAndMaybeSomethingElse) where
  fmap _ (Falsish) = Falsish
  fmap f (Truish a) = Truish (f a)


-- Crazy type: don't quite understand it
newtype Mu f = InF { outF :: f (Mu f) }


data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
  fmap f Finance = Finance
  fmap f (Desk x) = Desk x
  fmap f (Bloor b) = Bloor (f b)


data K a b = K a

instance Functor (K a) where
  fmap f (K x) = K x
  

data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap func (LiftItOut x) = LiftItOut (fmap func x)


data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap func (DaWrappa fa ga) = DaWrappa (fmap func fa) (fmap func ga)


data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap func (Notorious go ga gt) = Notorious go ga (fmap func gt)


data List a = Nil | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)


data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
  fmap f Halt = Halt
  fmap f (Print str x) = (Print str (f x))
  fmap f (Read fsa) = Read (f . fsa)



added :: Maybe Integer
added = 
  fmap (+3) (lookup 3 $ zip [1,2,3] [4,5,6])

yy :: Maybe Integer
yy = lookup 3 $ zip [1,2,3] [4,5,6]

zz :: Maybe Integer
zz = lookup 2 $ zip [1,2,3] [4,5,6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> yy <*> zz


xxx :: Maybe Int
xxx = elemIndex 3 [1,2,3,4,5]

yyy :: Maybe Int
yyy = elemIndex 4 [1,2,3,4,5]

max' :: Int -> Int -> Int
max'= max

maxed :: Maybe Int
maxed = max' <$> xxx <*> yyy


xxxs = [1, 2, 3]
yyys = [4, 5, 6]

x1 :: Maybe Integer
x1 = lookup 3 $ zip xxxs yyys

y1 :: Maybe Integer
y1 = lookup 2 $ zip xxxs yyys

summed :: Maybe Integer
summed = fmap sum $ (,) <$> x1 <*> y1


instance Applicative Main.Identity where
  pure = Main.Identity
  (<*>) (Main.Identity f) (Main.Identity a) = Main.Identity (f a)



newtype Constant a b = 
  Constant { getConstant :: a }
  deriving (Eq, Show, Ord)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

-- Note how you can define one mempty in terms of its "superclass".
instance Monoid a => Monoid (Constant a b) where
  mempty = Constant (mempty [])
  mappend (Constant x) (Constant y) = Constant (mappend x y)

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (<*>) x y = Constant (getConstant x <> getConstant y)



dummyHelloWord = const <$> Just "Hello" <*> Just "World"
dummyTierNess  = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1,2,3]



-- data List a = Nil | Cons a (List a)
instance Applicative List where
  pure x = Cons x Nil
  (<*>) _ Nil = Nil
  (<*>) Nil _ = Nil
  (<*>) (Cons f fs) vals = append (f <$> vals) (fs <*> vals)
    where
      append Nil ys = ys
      append (Cons x xs) ys = Cons x (append xs ys)



-------------------
  -- Monads
-------------------

bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join $ fmap f x

echo :: IO ()
echo = getLine >>= putStrLn


twiceWhenEven :: [Integer] -> [Integer]
-- twiceWhenEven xs = do
--   x <- xs
--   if even x
--      then [x*x, x*x]
--      else []
twiceWhenEven xs =
  xs >>=
    \x -> if even x
             then [x*x, x*x]
             else []


data Summ a b = FFirst a | SSecond b deriving (Eq, Show)

instance Functor (Summ a) where
  fmap f (SSecond x) = SSecond (f x)
  fmap _ (FFirst x) = FFirst x

instance Applicative (Summ a) where
  pure = SSecond
  (<*>) (SSecond f) (SSecond x) = SSecond (f x)
  (<*>) (FFirst x) _ = (FFirst x)
  (<*>) (SSecond _) (FFirst x) = (FFirst x)

instance Monad (Summ a) where
  return = pure
  (>>=) (SSecond x) f = f x
  (>>=) (FFirst x) _ = (FFirst x)


data Nope a = NopeDotJpg

instance Functor (Nope) where
  fmap _ _ = NopeDotJpg

instance Applicative (Nope) where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad (Nope) where
  return = pure
  (>>=) _ _ = NopeDotJpg


-- Identity defined above
instance Monad Main.Identity where
  return = pure
  (>>=) (Main.Identity x) f = f x


-- data List a = Nil | Cons a (List a)
instance Monad List where
  return = pure
  (>>=) Nil _ = Nil
  (>>=) (Cons c list) f = (f c) `concatlist` (list >>= f)
    where
      concatlist Nil list = list
      concatlist (Cons x tail) list = Cons x (concatlist tail list)



joiny :: Monad m => m (m a) -> m a
joiny x = x >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f x = (<*>) (fmap f x)

aaa :: Monad m => m a -> m (a -> b) -> m b
aaa = flip (<*>)


meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = do
  y <- f x
  fmap ((:) y) (meh xs f)
-- Alternative form
-- meh (x:xs) f = (f x) >>= \y -> fmap ((:) y) (meh xs f)

flipType :: (Monad m) => [m a] -> m [a]
flipType = (flip meh) id



mysum :: (Foldable t, Num a) => t a -> a
mysum x = getSum $ foldMap Sum x

myproduct :: (Foldable t, Num a) => t a -> a
myproduct x = getProduct $ foldMap Product x

myelem :: (Foldable t, Eq a) => a -> t a -> Bool
myelem e x = getAny $ foldMap (Any . (== e)) x


maybee :: (a -> a -> a) -> a -> Maybe a -> Maybe a
maybee f x Nothing = Just x
maybee f x (Just y) = Just (f x y)

myminimum :: (Foldable t, Ord a) => t a -> Maybe a
myminimum = foldr (maybee min) Nothing

mymaximum :: (Foldable t, Ord a) => t a -> Maybe a
mymaximum = foldr (maybee max) Nothing

mynull :: (Foldable t) => t a -> Bool
mynull = foldr (\_ _ -> False) True

mylength :: (Foldable t) => t a -> Int
mylength = foldr (\x y -> (y+1)) 0


---------------------------------------
    --
    -- Get Programming with Haskell
    --
---------------------------------------


cup flOz = \message -> message flOz


data Gender = Male | Female deriving Show
data RhType = Pos | Neg deriving Show
data ABOType = A | B | AB | O deriving Show
data BloodType = BloodType ABOType RhType deriving Show

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "A"
showABO O = "O"

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh) = showABO abo ++ showRh rh

data Patient = Patient { _name :: String
                       , _gender :: Gender
                       , _age :: Int
                       , _height :: Int
                       , _weight :: Int
                       , _bloodType :: BloodType } deriving (Show)

data Icecream = Vanilla | Chocolate deriving (Show, Eq, Ord)


myLast :: [a] -> a
myLast = head . reverse

myMin :: Ord a => [a] -> a
myMin = head . sort

myMax :: Ord a => [a] -> a
myMax = head . reverse . sort

myAll :: (a -> Bool) -> [a] -> Bool
myAll f = (foldr (&&) True) . (map f)

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = (foldr (||) False) . (map f)


instance S.Semigroup Integer where
    (<>) x y = x + y


data Box a = Box a deriving Show

wrap :: a -> Box a
wrap x = Box x

unwrap :: Box a -> a
unwrap (Box x) = x


data MyList a = Empty | MyCons a (MyList a)


-- Prime number generator
nums :: Integer -> [Integer]
nums m = pure (*) <*> [2..m] <*> [2..m]

primesToN :: Integer -> [Integer]
primesToN n = filter isNotComposite [2..n]
    where
        isNotComposite = not . (`elem` (nums n))


-- Generate all combinations of Users
data User = User 
    { thename :: String
    , gamerId :: Int
    , score :: Int
    } deriving Show

testNames :: [String]
testNames = ["Shafik Amin", "Robert'); DROP TABLE Students;--", "Chris NULL", "Randy"]
testIds :: [Int]
testIds = [1342, 342, 2423, 348]
testScores :: [Int]
testScores = [0, 10030, -4939]


-- Generates all permutations of even/odd pairs
-- between 1 .. n. Uses "do notation". Note
-- the lack of loops/recursion! Instead we rely on 
-- the List's monadic semantics.
allEvenOdds :: Int -> [(Int, Int)]
allEvenOdds n = do
    evenVal <- [2,4 .. n]
    oddVal <- [1,3 .. n]
    return (evenVal, oddVal)

-- De-sugared version of above!
allEvenOdds' :: Int -> [(Int, Int)]
allEvenOdds' n = [2,4 .. n] >>=
    (\evenVal -> [1,3 .. n] >>=
        (\oddVal -> return (evenVal, oddVal)))


-----------------
-- Back to Monads     
-----------------

add7 :: Int -> ClockNumber Int
add7 x = return (x+7)


class Monad m => ClockMonad m where
    (>>>=) :: (Integral a, Integral b) => m a -> (a -> m b) -> m b

data ClockNumber a = ClockNumber a deriving Show

instance Functor ClockNumber where
    fmap f (ClockNumber x) = ClockNumber (f x)

instance Applicative ClockNumber where
    pure = ClockNumber
    ClockNumber f <*> ClockNumber x = ClockNumber (f x)

instance Monad ClockNumber where
    return = ClockNumber
    (>>=) (ClockNumber x) f = f x

instance ClockMonad ClockNumber where
    (>>>=) (ClockNumber x) f = (f (x `mod` 12)) 


----------------

listSquared :: [Int] -> [Int]
listSquared list = do
    x <- list
    return (x^2);


hasDuplicates :: String -> Bool
hasDuplicates [] = False
hasDuplicates str = checkHead sorted
    where
        sorted = sort str
        checkHead [] = False
        checkHead [x] = False
        checkHead (x:xs) = (x == head xs) || checkHead xs


-- Same as hasDuplicates but with foldable Maybe(s)
hasDupes :: String -> Bool
hasDupes = isNothing . foldM checkHead 'X' . sort
    where
        checkHead :: Char -> Char -> Maybe Char
        checkHead x y
          | x == y = Nothing
          | otherwise = Just y

---------------------------------------------------
--- http://learnyouahaskell.com/a-fistful-of-monads
---------------------------------------------------

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole  
landLeft n (left,right)  
    | abs ((left + n) - right) < 4 = Just (left + n, right)  
    | otherwise = Nothing  
  
landRight :: Birds -> Pole -> Maybe Pole  
landRight n (left,right)  
    | abs (left - (right + n)) < 4 = Just (left, right + n)  
    | otherwise = Nothing  

-- Redefine a way to do function application
x -: f = f x

banana :: Pole -> Maybe Pole  
banana _ = Nothing

marySue :: Maybe Bool
marySue = do
  x <- Just 9
  Just (x > 8)

routine :: Maybe Pole
routine = do
  start <- return (0, 0)
  first <- landLeft 2 start
  second <- landRight 2 first
  landLeft 1 second

listOfTuples :: [(Int, Char)]
listOfTuples = do
  n <- [1, 2]
  ch <- ['a', 'b']
  return (n, ch)

isBigGang :: Int -> Bool
isBigGang = (> 9)

logNumber :: Int -> Writer [String] Int  
logNumber x = WriterT (DFI.Identity (x, ["Got number: " ++ show x]))
  
multWithLog :: Writer [String] Int  
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5  
    return (a*b+7)

-- Desugared version of above
multWithLog' :: Writer [String] Int
multWithLog' = 
  logNumber 3 >>= \a ->
    logNumber 5 >>= \b ->
      return (a*b+7)

-- This version of the above ignores the unwrapped
-- monadic values (3, 5, 8 below). Effectively, this
-- is the same as using the ">>" operator instead of 
-- the ">>=" operator
multWithLogIgnore :: Writer [String] Int
multWithLogIgnore = do
  logNumber 3
  logNumber 5
  logNumber 8
  return (77)

-- Desugared version of above
multWithLogIgnore' :: Writer [String] Int
multWithLogIgnore' = 
  logNumber 3 >> logNumber 5 >> logNumber 8 >> return 77

gcd' :: Int -> Int -> Int
gcd' a b
  | b == 0 = a
  | otherwise = gcd' b (a `mod` b)

gcdLogger :: Int -> Int -> Writer [String] Int
gcdLogger a b = do
  tell ["Recieved input " ++ show a ++ ", " ++ show b]
  return (gcd' a b)

threeCoins :: StdGen -> (Bool, Bool, Bool)  
threeCoins gen =   
    let (firstCoin, newGen) = random gen  
        (secondCoin, newGen') = random newGen  
        (thirdCoin, _) = random newGen'  
    in  (firstCoin, secondCoin, thirdCoin)


type Stack = [Int]

pop :: State Stack Int  
pop = state $ \(x:xs) -> (x,xs)
  
push :: Int -> State Stack ()  
push a = state $ \xs -> ((), a:xs)

stackManip :: State Stack Int  
stackManip = do  
    push 3  
    pop  
    pop

stackStuff :: State Stack ()
stackStuff = do
    x <- pop
    if x == 5
      then push 42
      else do
        push 13
        push 13
        push 13

randomSt :: (RandomGen g, Random a) => State g a  
randomSt = state random