module SetAssets
( Card
, Cards
, newDeck
, isSet
, anySets
, delete3
, groupsOf3
, sets
, cardnum
, Game(..)
) where

import Data.List

data Color  = Red      | Purple  | Green      deriving (Eq, Ord, Show, Read, Bounded, Enum) 
data Shape  = Squiggle | Diamond | Circle   deriving (Eq, Ord, Show, Read, Bounded, Enum) 
data Number = One      | Two     | Three      deriving (Eq, Ord, Show, Read, Bounded, Enum) 
data Shade  = Fill     | Hatch   | Empty       deriving (Eq, Ord, Show, Read, Bounded, Enum) 
data Card   = Card Number Shade Color Shape deriving (Eq, Ord, Show, Read) 

newDeck :: [Card]
newDeck = [ Card number shade color shape | shade  <- [Fill .. Empty]
                                           ,shape  <- [Squiggle .. Circle]
                                           ,color  <- [Red .. Green]
                                           ,number <- [One .. Three]]
type Cards = ([Card], [Card])

data Game = Game {
      players :: [String],
      deck :: Cards, -- ([Card], [Card])
      started :: Bool
} deriving Show

cardnum :: Card -> Int
cardnum c = let Just x = findIndex (\dc -> dc==c) newDeck in x+1

isSet :: [Card] -> Bool 
isSet []    = False
isSet cards = (sameOrDiff numbers) && (sameOrDiff shades) && (sameOrDiff colors) && (sameOrDiff shapes)
    where numbers = map (\(Card num _     _     _     ) -> num)   cards
          shades  = map (\(Card _   shade _     _     ) -> shade) cards 
          colors  = map (\(Card _   _     color _     ) -> color) cards 
          shapes  = map (\(Card _   _     _     shape ) -> shape) cards
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
