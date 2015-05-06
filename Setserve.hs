{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE GADTs, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}
import Yesod
import Yesod.Auth
import Yesod.Auth.Account
import qualified Yesod.Auth.Message as Msg
import Yesod.WebSockets

import Text.Julius
import Text.Lucius
import qualified Data.Text as T (Text, concat, pack, unpack)
import Data.ByteString (ByteString)

import Database.Persist.Sqlite
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Reader (void)
import Control.Monad (forever)
import Data.Monoid ((<>))
import Conduit
import Control.Concurrent.STM

import qualified Data.List as L (delete, intercalate, nub, findIndex, unlines)
import Data.Maybe (listToMaybe)

import System.Random (StdGen, getStdGen, next)
import System.Random.Shuffle (shuffle')

import SetAssets

-----------------Database/Yesod/Auth boilerplate----------------------------
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
User
    username T.Text
    UniqueUsername username
    password ByteString
    emailAddress T.Text
    verified Bool
    verifyKey T.Text
    resetPasswordKey T.Text
    deriving Show
|]

instance PersistUserCredentials User where
    userUsernameF = UserUsername
    userPasswordHashF = UserPassword
    userEmailF = UserEmailAddress
    userEmailVerifiedF = UserVerified
    userEmailVerifyKeyF = UserVerifyKey
    userResetPwdKeyF = UserResetPasswordKey
    uniqueUsername = UniqueUsername

    userCreate name _ key pwd = User name pwd "" True key ""

data MyApp = MyApp {
      cnpool :: ConnectionPool,
      games :: TVar [Game],
      nextGameId :: TVar Int,
      globChans :: [TChan T.Text],
      roomgids :: TVar  [Int]
}

mkYesod "MyApp" [parseRoutes|
/               HomeR GET
/game/room/#Int RoomR GET
/lobby          LobbyR GET
/auth           AuthR Auth getAuth
|]

instance Yesod MyApp

instance RenderMessage MyApp FormMessage where
    renderMessage _ _  = defaultFormMessage

instance YesodPersist MyApp where
    type YesodPersistBackend MyApp = SqlBackend
    runDB action = do
        myApp  <- getYesod
        runSqlPool action $ cnpool myApp 

instance YesodAuth MyApp where
    type AuthId MyApp = Username
    getAuthId = return . Just . credsIdent
    loginDest _ = HomeR
    logoutDest _ = HomeR
    authPlugins _ = [accountPlugin]
    authHttpManager _ = error "No manager needed"
    onLogin = return ()
    maybeAuthId = lookupSession credsKey

instance AccountSendEmail MyApp

instance YesodAuthAccount (AccountPersistDB MyApp User) MyApp where
    runAccountDB = runAccountPersistDB
    getNewAccountR = getNewAccountER
    postNewAccountR = postNewAccountOR
    getResetPasswordR = noResetR

    allowPasswordReset _ = False

-------------Real code begins------------------------------------------
main :: IO ()
main = do
  randomSeed   <- liftIO $ getStdGen >>= (\x -> return $ snd $ next x)
  theGames <- newTVarIO (gameStream randomSeed)
  firstGameId <- newTVarIO 11
  gameRoomIds <- newTVarIO [1..10]
  broadcastChannels <- sequence $  map atomically $ replicate 10 $ newBroadcastTChan

  runStderrLoggingT $ withSqlitePool "test.db3" 10 $ \pool -> do
    liftIO $ runSqlPool (runMigration migrateAll) pool
    liftIO $ warp 3000 $ MyApp pool theGames firstGameId broadcastChannels gameRoomIds


------------page request handler functions-----------------------------

-- just redirect to the lobby or login page.  
getHomeR :: Handler Html
getHomeR = do
    maid <- maybeAuthId
    case maid of
        Nothing -> redirect $ AuthR LoginR
        Just _ -> redirect $ LobbyR

--display all the gamerooms. TODO: auto-refresh the page every 5 seconds or something?
getLobbyR :: Handler Html
getLobbyR = do
  maid <- maybeAuthId
  myApp <- getYesod
  case maid of
    Nothing -> redirect $ AuthR LoginR
    Just u -> do 
              rgids <- liftIO $ readTVarIO (roomgids myApp)
              games <- sequence $ map getBoard rgids
              let gamenums = zip [1..] games
            --  liftIO $ putStrLn $ show $ (gamenums)
              defaultLayout $ [whamlet|
<p>Welcome to the lobby.
<p>You are logged in as #{u}
$forall game <- gamenums
                $if (started (snd game))
                    <p>Game in room #{fst game} has already begun. ( players: #{  show $ map fst $ players (snd game)    } )
                $else 
                     <p><a href="@{RoomR (fst game)}">Enter room #{fst game}</a> ( players: #{ show $ map fst $players (snd game)    } )
                    <p>Game not yet started.
                <p>-----------------------------

<p><a href="@{AuthR LogoutR}">Logout</a>
          |]

----------Account creation stuff.----------------------
getNewAccountER :: HandlerT Auth (HandlerT MyApp IO) Html
getNewAccountER = do
  tm <- getRouteToParent
  lift $ defaultLayout $ do
                   myjswid 
                   newAccountWidget tm

postNewAccountOR :: HandlerT Auth (HandlerT MyApp IO) Html
postNewAccountOR = do
        tm <- getRouteToParent
        mr <- lift getMessageRender
        ((result, _), _) <- lift $ runFormPost $ renderDivs newAccountForm
        mdata <- case result of
                    FormMissing -> invalidArgs ["Form is missing"]
                    FormFailure msg -> return $ Left msg
                    FormSuccess d -> return $ if newAccountPassword1 d == newAccountPassword2 d
                                        then Right d
                                        else Left [mr Msg.PassMismatch]
        case mdata of
            Left errs -> do
                setMessage $ toHtml $ T.concat errs
                redirect newAccountR

            Right d -> do void $ lift $ createNewAccount d tm
                          lift $ setMessageI $ Msg.LoginTitle
                          redirect LoginR

noResetR :: HandlerT Auth (HandlerT MyApp IO) Html
noResetR = do 
  tm <- getRouteToParent
  lift $ defaultLayout $  [whamlet|
<p>We don't collect emails! Your password cannot be reset. 
<p>
   <a href="@{tm newAccountR}">_{Msg.RegisterLong}
|]
  
-- Game room page and websockets handler. The meat of the game. 

-- NOTE: when manipulating the games (which are in shared memory) I
-- used pretty terrible naming conventions... og = old game,
-- ug=updated game, cg = current game, but what's "old", "updated",
-- and "current" is not at all consistent.
getRoomR :: Int -> Handler Html
getRoomR n = do
    myApp <- getYesod
    maid <- maybeAuthId
    case maid of
        Nothing -> redirect $ AuthR LoginR
        Just u -> do 
                currGames <- liftIO $ readTVarIO (roomgids myApp)
                webSockets (chatApp (currGames !! (n-1)) u n)
                defaultLayout $ do
                                $(whamletFile "gamepage.hamlet")
                                toWidget $(luciusFile "gamepage.lucius")
                                toWidget $(juliusFile "gamepage.julius")


chatApp :: Int -> T.Text -> Int -> WebSocketsT Handler ()
chatApp gid u rid  = do
  myApp <- getYesod
  let writeChan = (globChans myApp) !! (rid - 1)
  let wrCh txt = liftIO $ atomically $ writeTChan writeChan $ txt
  sendTextData ("CHATS: Welcome to Set, press deal to begin the game." :: T.Text)
  sendTextData $ T.pack ("DEBUG: gid: " ++ (show gid))
  readChan <- liftIO $ atomically $ do
                    writeTChan writeChan $ "CHATS: " <> u <> " has joined the chat"
                    dupTChan writeChan


  og <- getGame gid                                  
  let ug = Game { players = L.nub (((stripChars "\"" $ show u), 0):(players og)) , deck = deck og, started = started og}
  updateGame gid ug 
  cg <- getGame gid                                  
  wrCh (T.pack $ "PLAYR: " ++ (stripChars "\"" $ myshow  (players cg)))
  race_
                  (forever $ (liftIO $  atomically (readTChan readChan)) >>= sendTextData )
                  (sourceWS $$ mapM_C (\msg ->
                                           case msg of 
                                             "BEGIN"  -> do
                                                      og <- getGame gid
                                                      let ug = Game { players = players og, deck = deck og, started = True}
                                                      updateGame gid ug
                                                      wrCh msg
                                             "READY"  -> do 
                                                      cg <- getGame gid
                                                      playLoop gid (deck cg) writeChan u

                                                      --increment gamecounter and set rgid !! rid to the next game
                                                      liftIO $ newGame myApp
                                                      newId <- liftIO $ readTVarIO (nextGameId myApp)
                                                      currGames <- liftIO $ readTVarIO (roomgids myApp)
                                                      let newrids = replace' (rid - 1) newId currGames
                                                      _ <-  liftIO $ atomically $  modifyTVar (roomgids myApp) (\_ -> newrids)

                                                      wrCh "GOVER"
                                             _          -> wrCh "error"))

  forever $ (liftIO $  atomically (readTChan readChan)) >>= sendTextData 

    
playLoop :: Int -> Cards -> (TChan T.Text) -> T.Text ->  WebSocketsT Handler ()
playLoop gid (dealt, remaining) writeChan u
    | endGame    = do return ()
    | dealMore   = do
  liftIO $ putStrLn $ "Dealing more..."
  og <- getGame gid
  let ug = Game { players = L.nub (players og), 
                  deck =  (  (fst $ deck og) ++ (take 3 (snd $ deck og)), drop 3 (snd $ deck og) ),
                  started = started og 
                }
  updateGame gid ug
  cg <- getGame gid
  playLoop gid (fst $ deck cg, snd $ deck cg) writeChan u
     | otherwise  = do
    cg <- getGame gid
    wrCh (T.pack $ "PLAYR: " ++ (stripChars "\"" $ myshow  (players cg)))
    displayBoard
    sourceWS $$ mapM_C (\input -> do 
                          let mindices = maybeRead (T.unpack input)  
                          case mindices of 
                            Nothing -> do
                              wrCh ("DEBUG: No parse" :: T.Text)
                              ug <- getGame gid
                              playLoop gid (fst $ deck ug, snd $ deck ug) writeChan u
                            Just indices -> do
                                          let pickedSet = map (\i -> newDeck !! (i-1)) indices
                                          if isSet $ pickedSet 
                                          then do
                                            ug <- getGame gid
                                            if (pickedSet !! 0) `elem` (fst $ deck ug) 
                                            then do
                                              wrCh (T.pack $ "EVENT: " ++ (show u) ++ ",RIGHT")
                                              chPlayerScore (+1) (show u) gid
                                            else wrCh (T.pack $ "DEBUG: Double click by" ++ (show u))
                                            ug2 <- getGame gid
                                            let ng = Game { players = players ug2,
                                                            deck = (foldr L.delete (fst $ deck ug2) pickedSet, (snd $ deck ug2)),
                                                            started = started ug2
                                                          }
                                            liftIO $ putStrLn $ show (length $ snd $ deck ng)
                                            updateGame gid ng
                                            ng <- getGame gid
                                            playLoop gid (fst $ deck ng, snd $ deck ng) writeChan u
                                          else do
                                            wrCh (T.pack $ "EVENT: " ++ (show u) ++ ",WRONG")
                                            chPlayerScore (subtract 1) (show u) gid
                                            ug <- getGame gid
                                            playLoop gid (fst $ deck ug, snd $ deck ug) writeChan u)


    where dealMore = (not $ anySets dealt) || (length dealt < 12)
          endGame  = ((length $ remaining) == 0) && (not $ anySets dealt) 
          displayBoard = do 
            wrCh $ (T.pack "DEBUG: Current Board")
            wrCh $ (T.pack $ "CARDS" ++ (show $ map cardnum dealt))
            wrCh $ (T.pack $ "DEBUG: " ++ (show $ map cardnum dealt))
            wrCh $ (T.pack $ "NLEFT: " ++ (show $ length $ remaining))             
            wrCh (T.pack $ "DEBUG: " ++ (show dealt))
            wrCh (T.pack "DEBUG: sets on the board")
            wrCh (T.pack $ "DEBUG: " ++  (show $ sets dealt))


          wrCh txt = liftIO $ atomically $ writeTChan writeChan $ txt

chPlayerScore :: (Int -> Int) -> String -> Int -> WebSocketsT Handler ()
chPlayerScore f player gid = do
  game <- getGame gid
  let Just pindex = L.findIndex (\(name, _) -> (show name) == player) (players game)
  let new = (\(name, score) -> (name, f score)) ((players game) !! pindex)
  let updatedPlayers = replace' pindex new (players game) 
  let updatedGame = Game { players = updatedPlayers,
                           deck = deck game,
                           started = started game
                         }
  updateGame gid updatedGame
  
-- various helper functions 
-- TODO: refactor into separate file.        
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . filter (null . snd) . reads

updateGame :: Int -> Game -> WebSocketsT Handler ()
updateGame gid game = do 
  myApp <- getYesod
  _ <-  liftIO $ atomically $  modifyTVar (games myApp) (\gamess -> replace' gid (game) gamess)
  return ()

getGame :: Int -> WebSocketsT Handler (Game)
getGame gid = do
  myApp <- getYesod
  gameList <- liftIO $ readTVarIO $ games myApp
  return (gameList !! gid)
  
replace' :: Int -> a -> [a] -> [a]
replace' index element list = (take index list) ++ [element] ++ (drop (index+1) list)

newGame :: MyApp -> IO ()
newGame tfoo =
    atomically $  modifyTVar (nextGameId tfoo) incrementTVar
  where
    incrementTVar :: Int -> Int
    incrementTVar value = value+1

createGame :: StdGen -> Game
createGame rnd = let myDeck = splitAt 12 $ shuffle' newDeck (length newDeck) rnd
                 in Game {
                          players = [],
                          deck = myDeck,
                          started = False
                        }
  
gameStream :: StdGen -> [Game]
gameStream rnd = map (\gen -> createGame gen) infgens
    where infgens = scanl (\x f -> f x) rnd infn
          infn = repeat (snd . next)

getBoard :: Int -> HandlerT MyApp IO (Game)
getBoard gid = do
  myApp <- getYesod
  gameList <- liftIO $ readTVarIO $ games myApp
  return (gameList !! gid)

myjswid :: WidgetT MyApp IO ()
myjswid = do
  addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.11.2/jquery.min.js"
  setTitle "SET | Register"                
  toWidget [julius| 
                  $(document).ready(function(){
                                     
                                      $("#hident3").prop('value', 'user@set');
                                      $("#hident3").hide();
                                      $("label[for='hident3']").hide();
                  }); 
            |]


stripChars :: String -> String -> String
stripChars = filter . flip notElem


myshow :: [(String, Int)] -> String
myshow lst = concat $ map (\(name, score) -> name ++ ": " ++ (show score) ++ "<br>") lst
