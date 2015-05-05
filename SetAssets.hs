module SetAssets
( Card
, Cards
, Game(..)
, newDeck
, isSet
, anySets
, sets
, cardnum
) where

import Data.List

data Color  = Red      | Purple  | Green    deriving (Eq, Ord, Show, Read, Bounded, Enum) 
data Shape  = Squiggle | Diamond | Circle   deriving (Eq, Ord, Show, Read, Bounded, Enum) 
data Number = One      | Two     | Three    deriving (Eq, Ord, Show, Read, Bounded, Enum) 
data Shade  = Fill     | Hatch   | Empty    deriving (Eq, Ord, Show, Read, Bounded, Enum) 
data Card   = Card Number Shade Color Shape deriving (Eq, Ord, Show, Read) 

type Cards = ([Card], [Card])

data Game = Game {
      players :: [(String, Int)],
      deck :: Cards,
      started :: Bool
} deriving Show

newDeck :: [Card]
newDeck = [ Card number shade color shape | shade  <- [Fill .. Empty]
                                           ,shape  <- [Squiggle .. Circle]
                                           ,color  <- [Red .. Green]
                                           ,number <- [One .. Three]]

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

cardnum :: Card -> Int
cardnum c = let Just x = findIndex (\dc -> dc==c) newDeck in x+1

--interal helper functions, not exported.
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs, ys <- combinations (n-1) xs']

