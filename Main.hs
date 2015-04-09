{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE GADTs, MultiParamTypeClasses, TypeSynonymInstances #-}

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
import System.Random
import Text.Julius
import Text.Lucius
import qualified Text.Read as TR (read)
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

data MyApp = MyApp ConnectionPool (TChan T.Text)

mkYesod "MyApp" [parseRoutes|
/ HomeR GET
/game GameR GET
/lobby LobbyR GET
/auth AuthR Auth getAuth
|]

instance Yesod MyApp

instance RenderMessage MyApp FormMessage where
    renderMessage _ _  = defaultFormMessage

instance YesodPersist MyApp where
    type YesodPersistBackend MyApp = SqlBackend
    runDB action = do
        MyApp pool _ <- getYesod
        runSqlPool action pool

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
        Just u -> redirect $ LobbyR



getLobbyR :: Handler Html
getLobbyR = do
  maid <- maybeAuthId
  case maid of
    Nothing -> redirect $ AuthR LoginR
    Just u -> defaultLayout $ [whamlet|
<p>Welcome to the lobby.
<p>You are logged in as #{u}
<p><a href="@{GameR}">Begin the game</a>
<p><a href="@{AuthR LogoutR}">Logout</a>
    |]


getNewAccountER :: HandlerT Auth (HandlerT MyApp IO) Html
getNewAccountER = do
  tm <- getRouteToParent
  lift $ defaultLayout $ do
                   myjswid 
                   newAccountWidget tm

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

main :: IO ()
main = runStderrLoggingT $ withSqlitePool "test.db3" 10 $ \pool -> do
    liftIO $ runSqlPool (runMigration migrateAll) pool
    chan <- liftIO $ atomically newBroadcastTChan
    liftIO $ warp 3000 $ MyApp pool chan



getGameR :: Handler Html
getGameR = do
    webSockets chatApp
    maid <- maybeAuthId
    case maid of
        Nothing -> redirect $ AuthR LoginR
        Just u -> defaultLayout $ do
                    $(whamletFile "gamepage.hamlet")
                    toWidget $(luciusFile "gamepage.lucius")
                    toWidget $(juliusFile "gamepage.julius")

chatApp :: WebSocketsT Handler ()
chatApp = do
    sendTextData ("Welcome to Set, please enter your name." :: T.Text)
    name <- receiveData
    sendTextData $ "Welcome, " <> name
    MyApp _ writeChan <- getYesod

    readChan <- liftIO $ atomically $ do
        writeTChan writeChan $ name <> " has joined the chat"
        dupTChan writeChan


    let (dealt, remaining) = splitAt 12 $ getDeck (mkStdGen 100)
    playLoop (dealt, remaining)
            


playLoop (dealt, remaining)
    | endGame    = do return ()
    | dealMore   = playLoop (dealt ++ (take 3 remaining), drop 3 remaining)
    | otherwise  = do
                 -- sendTextData (T.pack "Current Board")
                 displayBoard
                -- sendTextData (T.pack "sets on the board")
                -- sendTextData (T.pack $ show $ sets dealt)
                 
                 input <- receiveData
                 let indices = read (T.unpack input)  --add input checking
                 let pickedSet = zipWith (!!) (replicate 3 dealt) indices 
                 if isSet $ pickedSet 
                 then do
                  -- sendTextData ("Correct" :: T.Text)
                   playLoop  (delete3 indices dealt, remaining)
                 else do
                   --sendTextData ("Wrong" :: T.Text)
                   playLoop  (dealt, remaining)

    where dealMore = (not $ anySets dealt) || (length dealt < 12)
          endGame  = ((length $ remaining) == 0) && (not $ anySets dealt) 
          displayBoard = sendTextData (T.pack $ init $ tail $ show $ map cardnum dealt)

