import Data.List
import System.Random
import SetAssets

main = do
    let (dealt, remaining) = splitAt 12  $ getDeck (mkStdGen 81)
    playLoop (dealt, remaining)

playLoop (dealt, remaining)
    | endGame    = do return ()
    | dealMore   = playLoop (dealt ++ (take 3 remaining), drop 3 remaining)
    | otherwise  = do
                 putStrLn "Current Board" 
                 displayBoard
                 putStrLn $ "Cards on board: " ++ (show $ length dealt)
                 putStrLn $ "Cards on in deck: " ++ (show $ length remaining)
                 print $ sets dealt
                 input <- sequence [getLine, getLine, getLine]
                 let indices = map read input  --add input checking
                 let pickedSet = zipWith (!!) (replicate 3 dealt) indices 

                 if isSet $ pickedSet 
                 then do
                  putStrLn "match" --keep score here
                  playLoop (delete3 indices dealt, remaining) 
                 else do
                  putStrLn "wrong" 
                  playLoop (dealt, remaining)

    where dealMore = (not $ anySets dealt) || (length dealt < 12)
          endGame  = ((length $ remaining) == 0) && (not $ anySets dealt) 
          displayBoard = sequence $ map print $ groupsOf3 dealt

