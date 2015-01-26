import Data.List
import Data.Random 
import Data.Random.Source.Std

data Color  = Red    | Green   | Purple     deriving (Eq, Ord, Show, Read, Bounded, Enum) 
data Shape  = Circle | Diamond | Squiggle   deriving (Eq, Ord, Show, Read, Bounded, Enum) 
data Number = One    | Two     | Three      deriving (Eq, Ord, Show, Read, Bounded, Enum) 
data Shade  = Empty  | Hatch   | Fill       deriving (Eq, Ord, Show, Read, Bounded, Enum) 
data Card   = Card Number Shade Color Shape deriving (Eq, Ord, Show, Read) 

getNum   (Card num _     _     _     ) = num
getShade (Card _   shade _     _     ) = shade
getColor (Card _   _     color _     ) = color
getShape (Card _   _     _     shape ) = shape

newdeck = [ Card number shade color shape | number <- [One .. Three]
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

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs, ys <- combinations (n-1) xs']

main = do
    deck <- runRVar (shuffle newdeck) StdRandom
    let (dealt, remaining) = splitAt 12 deck
    playLoop (dealt, remaining)

playLoop (dealt, remaining)
    | endGame (dealt, remaining) = do return ()
    | dealMore dealt             = playLoop (dealt ++ (take 3 remaining), drop 3 remaining)
    | otherwise                  = do
                                 putStrLn "Current Board" 
                                 print $ dealt
                                 putStrLn $ "Cards on board: " ++ (show $ length dealt)
                                 putStrLn $ "Cards on in deck: " ++ (show $ length remaining)
                                 --print $ sets dealt
                                 input <- sequence [getLine, getLine, getLine]
                                 let indices = map read input  --add input checking here

                                 if isSet $ zipWith (!!) (replicate 3 dealt) indices 
                                 then do
                                  putStrLn "match" --keep score here
                                  playLoop (delete3 indices dealt, remaining) 
                                 else do
                                  putStrLn "wrong" 
                                  playLoop (delete3 indices dealt, remaining) 

    where dealMore dealt = (not $ anySets dealt) || (length dealt < 12)
          endGame (dealt, remaining) = ((length $ remaining) == 0) && (not $ anySets dealt) 

delete3 indices dealt = foldr delete' dealt indices

delete' :: Int -> [a] -> [a]
delete' index list = let (front, back) = splitAt index list
                      in front ++ (tail back)

