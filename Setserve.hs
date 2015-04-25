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
import qualified Data.List as L (delete)


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
      channel :: TChan T.Text,
      deck :: Cards
}

data MyApp = MyApp {
      cnpool :: ConnectionPool,
      games :: MVar [IO Game],
      nextGameId :: MVar Int,
      globChan :: TChan T.Text
}

newGame :: MyApp -> IO Int
newGame tfoo =
    modifyMVar (nextGameId tfoo) incrementMVar
  where
    incrementMVar :: Int -> IO (Int, Int)
    incrementMVar value = return (value+1, value)
 


mkYesod "MyApp" [parseRoutes|
/          HomeR GET
/game/#Int GameR GET
/game      GamesR POST
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



getLobbyR :: Handler Html
getLobbyR = do
  maid <- maybeAuthId
  case maid of
    Nothing -> redirect $ AuthR LoginR
    Just u -> defaultLayout $ [whamlet|
<p>Welcome to the lobby.
<p>You are logged in as #{u}
<form method=post action=@{GamesR}>
              <input type=submit value="VS. Human">

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
                                     
                                      $("#hident3").prop('value', 'unused@unused');
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





createGame :: StdGen -> IO Game
createGame rnd = do
  newchan <- liftIO $ atomically newBroadcastTChan
  let myDeck = ([], shuffle' newDeck (length newDeck) rnd)
  return Game {
               players = [],
               channel = newchan,
               deck = myDeck
             }
  

gameStream :: StdGen -> [IO Game]
gameStream rnd = map (\gen -> createGame gen) infgens
    where infgens = scanl (\x f -> f x) rnd infn
          infn = repeat (snd . next)

main :: IO ()
main = do
  seedP   <- liftIO $ Random.getStdGen >>= (\x -> return $ snd $ next x)
  theGames <- newMVar (gameStream seedP)
  firstGameId <- newMVar 1
  globalChan <- liftIO $ atomically newBroadcastTChan
  runStderrLoggingT $ withSqlitePool "test.db3" 10 $ \pool -> do
    liftIO $ runSqlPool (runMigration migrateAll) pool
    liftIO $ warp 3000 $ MyApp pool theGames firstGameId globalChan


postGamesR :: Handler Html
postGamesR = do
  myApp <- getYesod  
  gid <- liftIO $ newGame myApp
  redirect $ GameR gid

getGame :: Int -> WebSocketsT Handler Game
getGame gid = do
  tfoo <- getYesod
  maxId <- liftIO $ readMVar $ nextGameId tfoo
  list  <- liftIO $ readMVar $ games tfoo
  if gid < maxId
    then (liftIO $ (list) !! gid) >>= (\game -> return game)
    else notFound


getGameR :: Int -> Handler Html
getGameR gid = do
    maid <- maybeAuthId
    case maid of
        Nothing -> redirect $ AuthR LoginR
        Just u -> do 
                webSockets (chatApp gid u)
                defaultLayout $ do
                    $(whamletFile "gamepage.hamlet")
                    toWidget $(luciusFile "gamepage.lucius")
                    toWidget $(juliusFile "gamepage.julius")

chatApp :: Int -> T.Text -> WebSocketsT Handler ()
chatApp gid u  = do
  game <- getGame gid
  myApp <- getYesod
  let writeChan = globChan myApp
  sendTextData ("CHATS: Welcome to Set, press deal to begin the game." :: T.Text)
  readChan <- liftIO $ atomically $ do
                    writeTChan writeChan $ "CHATS: " <> u <> " has joined the chat"
                    dupTChan writeChan
  let (_, remaining) = deck game
  --(dealt, remaining) <- liftIO $ IO (deck game)
  sendTextData (T.pack $ "DEBUG: (deck1) " ++ (show $ remaining))

  game <- getGame gid
  let (_, remaining) = deck game
  --(dealt, remaining) <- liftIO $ deck game
  sendTextData (T.pack $ "DEBUG: (deck2) " ++ (show $ remaining))
  

                                  
  let (firstDeal, firstRem) = splitAt 12 remaining
  let wrCh txt = liftIO $ atomically $ writeTChan writeChan $ txt
  race_
                  (forever $ (liftIO $  atomically (readTChan readChan)) >>= sendTextData )
                  (sourceWS $$ mapM_C (\msg ->
                                           case msg of 
                                             "BEGIN"  -> wrCh msg
                                             "READY"  -> playLoop (firstDeal, firstRem) writeChan
                                             _          -> wrCh "error"))

    
        

playLoop :: Cards -> (TChan T.Text) ->  WebSocketsT Handler ()
playLoop (dealt, remaining) writeChan
    | endGame    = do return ()
    | dealMore   = playLoop (dealt ++ (take 3 remaining), drop 3 remaining) writeChan
    | otherwise  = do
                 wrCh $ (T.pack "DEBUG: Current Board")
                 displayBoard
                 wrCh (T.pack $ "DEBUG: " ++ (show dealt))
                 wrCh (T.pack "DEBUG: sets on the board")
                 wrCh (T.pack $ "DEBUG: " ++  (show $ sets dealt))
                 sourceWS $$ mapM_C (\input -> do 
                                     let indices = read (T.unpack input)  --add input checking
                                     let pickedSet = map (\i -> newDeck !! (i-1)) indices
                                     if isSet $ pickedSet 
                                     then do
                                       wrCh ("DEBUG: Correct" :: T.Text)
                                       playLoop  (foldr L.delete dealt pickedSet, remaining) writeChan
                                     else do
                                       wrCh ("DEBUG: Wrong" :: T.Text)
                                       playLoop  (dealt, remaining) writeChan)

    where dealMore = (not $ anySets dealt) || (length dealt < 12)
          endGame  = ((length $ remaining) == 0) && (not $ anySets dealt) 
          displayBoard = do 
            wrCh $ (T.pack $ "CARDS" ++ (show $ map cardnum dealt))
            wrCh $ (T.pack $ "DEBUG: " ++ (show $ map cardnum dealt))
            wrCh $ (T.pack $ "DEBUG: " ++ (show $ length $ remaining)) 
          wrCh txt = liftIO $ atomically $ writeTChan writeChan $ txt
        
