module SetAssets
( Card
, newDeck
, isSet
, anySets
, delete3
, groupsOf3
, sets
) where

import Data.List

data Color  = Red    | Green   | Purple     deriving (Eq, Ord, Show, Read, Bounded, Enum) 
data Shape  = Circle | Diamond | Squiggle   deriving (Eq, Ord, Show, Read, Bounded, Enum) 
data Number = One    | Two     | Three      deriving (Eq, Ord, Show, Read, Bounded, Enum) 
data Shade  = Empty  | Hatch   | Fill       deriving (Eq, Ord, Show, Read, Bounded, Enum) 
data Card   = Card Number Shade Color Shape deriving (Eq, Ord, Show, Read) 

getNum   (Card num _     _     _     ) = num
getShade (Card _   shade _     _     ) = shade
getColor (Card _   _     color _     ) = color
getShape (Card _   _     _     shape ) = shape

newDeck = [ Card number shade color shape | number <- [One .. Three]
                                           ,shade  <- [Empty .. Fill]
                                           ,color  <- [Red .. Purple]
                                           ,shape  <- [Circle .. Squiggle]]

isSet :: [Card] -> Bool 
isSet cards = (sameOrDiff numbers) && (sameOrDiff shades) && (sameOrDiff colors) && (sameOrDiff shapes)
		where numbers = map getNum cards
		      shades  = map getShade cards 
		      colors  = map getColor cards 
		      shapes  = map getShape cards
		      sameOrDiff lst = or [(length $ nub lst) == 1, (length $ nub lst) == (length lst) ]  

sets :: [Card] -> [[Card]]
sets cards  = filter isSet $ combinations 3 cards

anySets :: [Card] -> Bool
anySets cards = (length $ sets cards) > 0

delete3 :: [Int] -> [Card] -> [Card]
delete3 indices dealt = foldr deleteIndex dealt (sort indices)

deleteIndex :: Int -> [a] -> [a]
deleteIndex index list = let (front, back) = splitAt index list
                      in front ++ (tail back)

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs, ys <- combinations (n-1) xs']

groupsOf3 :: [a] -> [[a]]  
groupsOf3 []  = []     
groupsOf3 xs = take 3 xs : groupsOf3 (drop 3 xs)  
