{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE GADTs, MultiParamTypeClasses, TypeSynonymInstances #-}

import Data.Text (Text)
import Data.ByteString (ByteString)
import Database.Persist.Sqlite
import Control.Monad.Logger (runStderrLoggingT)
import Yesod
import Yesod.Auth
import Yesod.Auth.Account

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
User
    username Text
    UniqueUsername username
    password ByteString
    emailAddress Text
    verified Bool
    verifyKey Text
    resetPasswordKey Text
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

data MyApp = MyApp ConnectionPool

mkYesod "MyApp" [parseRoutes|
/ HomeR GET
/auth AuthR Auth getAuth
|]

instance Yesod MyApp

instance RenderMessage MyApp FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodPersist MyApp where
    type YesodPersistBackend MyApp = SqlBackend
    runDB action = do
        MyApp pool <- getYesod
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

getHomeR :: Handler Html
getHomeR = do
    maid <- maybeAuthId
    case maid of
        Nothing -> redirect $ AuthR LoginR
        Just u -> defaultLayout $ [whamlet|
<p>You are logged in as #{u}
<p><a href="@{AuthR LogoutR}">Logout</a>
|]


getNewAccountER :: HandlerT Auth (HandlerT MyApp IO) Html
getNewAccountER = do
  tm <- getRouteToParent
  lift $ defaultLayout $ do
                   myjswid 
                   newAccountWidget tm


main :: IO ()
main = runStderrLoggingT $ withSqlitePool "test.db3" 10 $ \pool -> do
    runSqlPool (runMigration migrateAll) pool
    liftIO $ warp 3000 $ MyApp pool


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


