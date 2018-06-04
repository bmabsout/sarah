module Convenience
(module Convenience, (&)) where

import Data.Function
import qualified Data.Set as S
import Data.List
-- import Data.Matrix


infixl 2 &.
(&.) :: (a -> b) -> (b -> c) -> a -> c
(&.) = flip (.)

infixl 3 &.>
(&.>) :: Functor f => (a -> f b) -> (b -> c) -> a -> f c
(&.>) g f = (fmap f).g

infixl 1 &>
(&>) :: Functor f => f a -> (a -> b) -> f b
(&>) = flip (<$>)

infixl 1 &>>
(&>>) :: (Functor f,Functor g) => f (g a) -> (a -> b) -> f (g b)
(&>>) = flip (fmap.fmap)

ifd :: Bool -> (a->a) -> (a->a)
ifd b f = if b then f else id

if' :: a -> a -> Bool -> a
if' x _ True  = x
if' _ y False = y

sortAndNub :: (Ord a) => [a] -> [a]
sortAndNub = go S.empty
  where go _ [] = []
        go s (x:xs) | S.member x s = go s xs
                    | otherwise    = x : go (S.insert x s) xs

nub' :: (Ord a) => [a] -> [a]
nub' = S.fromList &. S.toList

ofLength :: Integral b => b -> [a] -> Bool
ofLength 0 []     = True
ofLength 0 _      = False
ofLength _ []     = False
ofLength k (_:xs) = ofLength (k - 1) xs

subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize n xs = let l = length xs
                          in if n>l then [] else subsequencesBySize xs !! (l-n)
 where
   subsequencesBySize [] = [[[]]]
   subsequencesBySize (y:ys) = let next = subsequencesBySize ys
                             in zipWith (++) ([]:next) (map (map (y:)) next ++ [[]])

transposeCut :: [[a]] -> [[a]]
transposeCut [] = []
transposeCut l
    | any null l = []
    | otherwise = (l &> head) : transposeCut (l &> tail)

window :: (Eq a ,Integral b) => ([a] -> c) -> b -> [a] -> [c]
window f n l = tails l & take (fromIntegral n)
                       & transposeCut
                       &> f

-- matrixWindow :: Int -> Int -> Matrix Int -> [Matrix Int]
-- matrixWindow ww wh mat = [submatrix currH (currH+wh-1) currW (currW+ww-1) mat | currW <- [1..w], currH <- [1..h]]
--     where w = ncols mat - ww + 1
--           h = nrows mat - wh + 1


readTwo :: (Read a, Read b) => IO (a,b)
readTwo = getLine &> words &. \(a:b:_) -> (read a, read b)

readThree :: (Read a, Read b, Read c) => IO (a,b,c)
readThree = getLine &> words &. \(a:b:c:_) -> (read a, read b, read c)

readWords :: Read a => IO [a]
readWords = getLine &> words &.> read
