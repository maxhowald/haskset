{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE GADTs, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}
import qualified Data.Text as T (Text, concat, pack, unpack)
import Data.ByteString (ByteString)
import Database.Persist.Sqlite
import Control.Monad.Logger (runStderrLoggingT)
import Yesod
import Yesod.Auth
import Yesod.Auth.Account
import Yesod.Auth.Message

import Control.Applicative
import qualified Yesod.Auth.Message as Msg
import Control.Monad.Reader (void)

import Yesod.Core
import Yesod.WebSockets
import qualified Data.Text.Lazy as TL
import Control.Monad (forever)
import Control.Monad.Trans.Reader
import Control.Concurrent (threadDelay)
import Data.Time
 -- import Conduit
import Data.Monoid ((<>))
import Control.Concurrent.STM

import SetAssets

import Text.Julius
import Text.Lucius
import qualified Text.Read as TR (read)
import qualified Data.List as L (delete, intercalate, nub)
import Data.Maybe (listToMaybe)

import Conduit
import Control.Concurrent.MVar

import System.Random as Random
import System.Random.Shuffle (shuffle')
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

    userCreate name email key pwd = User name pwd "" True key ""


data Game = Game {
      players :: [String],
      deck :: Cards
}

data MyApp = MyApp {
      cnpool :: ConnectionPool,
      games :: MVar [Game],
      nextGameId :: MVar Int,
      globChans :: [TChan T.Text],
      room1gid :: MVar  Int,
      room2gid :: MVar  Int
}

newGame :: MyApp -> IO Int
newGame tfoo =
    modifyMVar (nextGameId tfoo) incrementMVar
  where
    incrementMVar :: Int -> IO (Int, Int)
    incrementMVar value = return (value+1, value)
 


mkYesod "MyApp" [parseRoutes|
/          HomeR GET
/game/room1 Room1R GET
/game/room2 Room2R GET
/lobby     LobbyR GET
/auth      AuthR Auth getAuth
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


getHomeR :: Handler Html
getHomeR = do
    maid <- maybeAuthId
    case maid of
        Nothing -> redirect $ AuthR LoginR
        Just _ -> redirect $ LobbyR



getBoard :: Int -> HandlerT MyApp IO (Cards, [String])
getBoard gid = do
  myApp <- getYesod
  gameList <- liftIO $ readMVar $ games myApp
  let game = gameList !! gid
  return (deck game, players game)


getLobbyR :: Handler Html
getLobbyR = do
  maid <- maybeAuthId
  myApp <- getYesod
  case maid of
    Nothing -> redirect $ AuthR LoginR
    Just u -> do 
              r1 <- liftIO $ readMVar (room1gid myApp)
              r2 <- liftIO $ readMVar (room2gid myApp)
              ((deal1,rem1),pl1) <- getBoard r1
              ((deal2,rem2),pl2) <- getBoard r2
              let c1 = (length deal1) + (length rem1)
              let c2 = (length deal2) + (length rem2)
              let p1 = T.pack $ L.intercalate ", " pl1
              let p2 = T.pack $ L.intercalate ", " pl2
              defaultLayout $ [whamlet|
<p>Welcome to the lobby.
<p>You are logged in as #{u}
<p><a href="@{Room1R}">Enter room 1</a> (players: #{p1}) (cards left in play: #{c1})
<p><a href="@{Room2R}">Enter room 2</a> (players: #{p2}) (cards left in play: #{c2})


<p><a href="@{AuthR LogoutR}">Logout</a>
          |]


getNewAccountER :: HandlerT Auth (HandlerT MyApp IO) Html
getNewAccountER = do
  tm <- getRouteToParent
  lift $ defaultLayout $ do
                   myjswid 
                   newAccountWidget tm

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





createGame :: StdGen -> Game
createGame rnd = let myDeck = splitAt 12 $ shuffle' newDeck (length newDeck) rnd
                 in Game {
                          players = [],
                          deck = myDeck
                        }
  

gameStream :: StdGen -> [Game]
gameStream rnd = map (\gen -> createGame gen) infgens
    where infgens = scanl (\x f -> f x) rnd infn
          infn = repeat (snd . next)

main :: IO ()
main = do
  seedP   <- liftIO $ Random.getStdGen >>= (\x -> return $ snd $ next x)
  theGames <- newMVar (gameStream seedP)
  firstGameId <- newMVar 1
  r1g <- newMVar 1
  r2g <- newMVar 2
  let globalChan =  map atomically $ replicate 2 $ newBroadcastTChan
  globchan1 <- (globalChan !! 0)
  globchan2 <- (globalChan !! 1)
  runStderrLoggingT $ withSqlitePool "test.db3" 10 $ \pool -> do
    liftIO $ runSqlPool (runMigration migrateAll) pool
    liftIO $ warp 3000 $ MyApp pool theGames firstGameId [globchan1, globchan2] r1g r2g


  

getRoom1R :: Handler Html
getRoom1R = do
    myApp <- getYesod
    maid <- maybeAuthId
    case maid of
        Nothing -> redirect $ AuthR LoginR
        Just u -> do 
                currGame <- liftIO $ readMVar (room1gid myApp)
                webSockets (chatApp currGame u 1)
                defaultLayout $ do
                                $(whamletFile "gamepage.hamlet")
                                toWidget $(luciusFile "gamepage.lucius")
                                toWidget $(juliusFile "gamepage.julius")

getRoom2R :: Handler Html
getRoom2R = do
    myApp <- getYesod
    maid <- maybeAuthId
    case maid of
        Nothing -> redirect $ AuthR LoginR
        Just u -> do 
                currGame <- liftIO $ readMVar (room2gid myApp)
                webSockets (chatApp currGame u 2)
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


  ((newDeal, newRem), gplayers) <- refBoard gid                                  
  let ug = Game { players = L.nub ((show u):gplayers) , deck = (newDeal, newRem)}
  updateGame gid ug 
  ((newDeal, newRem), gplayers) <- refBoard gid                                  
  wrCh (T.pack $ "PLAYR: " ++ (L.intercalate ", " gplayers))
  
  race_
                  (forever $ (liftIO $  atomically (readTChan readChan)) >>= sendTextData )
                  (sourceWS $$ mapM_C (\msg ->
                                           case msg of 
                                             "BEGIN"  -> do
                                                      wrCh msg
                                             "READY"  -> do 
                                                      ((firstDeal, firstRem), _) <- refBoard gid
                                                      playLoop gid (firstDeal, firstRem) writeChan u
                                                      wrCh "GOVER"
                                             _          -> wrCh "error"))

    
        

playLoop :: Int -> Cards -> (TChan T.Text) -> T.Text ->  WebSocketsT Handler ()
playLoop gid (dealt, remaining) writeChan u
    | endGame    = do return ()
    | dealMore   = do
  ((newDeal, newRem), newplayers) <- refBoard gid
  let ug = Game { players = L.nub newplayers, deck =  (newDeal ++ (take 3 newRem), drop 3 newRem) }
  updateGame gid ug
  ((newDeal, newRem), gplayers) <- refBoard gid
  playLoop gid (newDeal, newRem) writeChan u
     | otherwise  = do
    ((_, _), gplayers) <- refBoard gid
    wrCh (T.pack $ "PLAYR: " ++ (L.intercalate ", " gplayers))
    displayBoard
    sourceWS $$ mapM_C (\input -> do 
                          let mindices = maybeRead (T.unpack input)  
                          case mindices of 
                            Nothing -> do
                              wrCh ("DEBUG: No parse" :: T.Text)
                              ((newDeal, newRem), players) <- refBoard gid
                              playLoop gid  (newDeal, newRem) writeChan u
                            Just indices -> do
                                          let pickedSet = map (\i -> newDeck !! (i-1)) indices
                                          if isSet $ pickedSet 
                                          then do
                                            wrCh (T.pack $ "EVENT: " ++ (show u) ++ ",CORRECT")
                                            ((newDeal, newRem), cplayers) <- refBoard gid
                                            let ug = Game { players = L.nub cplayers, deck = (foldr L.delete newDeal pickedSet, newRem) }
                                            updateGame gid ug
                                            ((newDeal, newRem), players) <- refBoard gid
                                            playLoop gid  (newDeal, newRem) writeChan u
                                          else do
                                            wrCh (T.pack $ "EVENT: " ++ (show u) ++ ",WRONG")
                                            ((newDeal, newRem), players) <- refBoard gid
                                            playLoop gid  (newDeal, newRem) writeChan u) 


    where dealMore = (not $ anySets dealt) || (length dealt < 12)
          endGame  = ((length $ remaining) == 0) && (not $ anySets dealt) 
          displayBoard = do 
            wrCh $ (T.pack "DEBUG: Current Board")
            wrCh $ (T.pack $ "CARDS" ++ (show $ map cardnum dealt))
            wrCh $ (T.pack $ "DEBUG: " ++ (show $ map cardnum dealt))
            wrCh $ (T.pack $ "DEBUG: " ++ (show $ length $ remaining))             
            wrCh (T.pack $ "DEBUG: " ++ (show dealt))
            wrCh (T.pack "DEBUG: sets on the board")
            wrCh (T.pack $ "DEBUG: " ++  (show $ sets dealt))


          wrCh txt = liftIO $ atomically $ writeTChan writeChan $ txt
        
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . filter (null . snd) . reads


updateGame :: Int -> Game -> WebSocketsT Handler ()
updateGame gid game = do 
  myApp <- getYesod
  _ <-  liftIO $ modifyMVar (games myApp) (\gamess ->
                                     return (replace' gid (game) gamess, gamess))
  return ()



refBoard :: Int -> WebSocketsT Handler (Cards, [String])
refBoard gid = do
  myApp <- getYesod
  gameList <- liftIO $ readMVar $ games myApp
  let game = gameList !! gid
  return (deck game, players game)



replace' :: Int -> a -> [a] -> [a]
replace' index element list = (take index list) ++ [element] ++ (drop (index+1) list)
