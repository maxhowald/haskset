{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
import Yesod.Core
import Yesod.WebSockets
import qualified Data.Text.Lazy as TL
import Control.Monad (forever)
import Control.Monad.Trans.Reader
import Control.Concurrent (threadDelay)
import Data.Time
import Conduit
import Data.Monoid ((<>))
import Control.Concurrent.STM.Lifted
import Data.Text (Text, pack)
import SetAssets
import qualified Data.Text as T
data App = App (TChan Text)

instance Yesod App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

chatApp :: WebSocketsT Handler ()
chatApp = do
    sendTextData ("Welcome to Set. Enter your name to begin." :: Text)
    name <- receiveData
    sendTextData $ "Welcome, " <> name
    App writeChan <- getYesod

    readChan <- atomically $ dupTChan writeChan     
    atomically $ writeTChan writeChan $ name <> " has joined."

    let deck = debugDeal
    let (dealt, remaining) = splitAt 12 $ deck

    race_
          (forever $ atomically (readTChan readChan) >>= sendTextData)
          (sourceWS $$ mapM_C (f writeChan name dealt))




f writeChan name dealt msg = if (msg == "start")
                             then do atomically $ writeTChan writeChan $ T.pack $ show dealt 
                             else  do atomically $ writeTChan writeChan $ name <> ": " <> msg
          

getHomeR :: Handler Html
getHomeR = do
    webSockets chatApp
    defaultLayout $ do
        [whamlet|
            <div #output>
            <form #form>
                <input #input autofocus>
        |]
        toWidget [lucius|
            \#output {
                width: 600px;
                height: 400px;
                border: 1px solid black;
                margin-bottom: 1em;
                p {
                    margin: 0 0 0.5em 0;
                    padding: 0 0 0.5em 0;
                    border-bottom: 1px dashed #99aa99;
                }
            }
            \#input {
                width: 600px;
                display: block;
            }
        |]
        toWidget [julius|
            var url = document.URL,
                output = document.getElementById("output"),
                form = document.getElementById("form"),
                input = document.getElementById("input"),
                conn;

            url = url.replace("http:", "ws:").replace("https:", "wss:");
            conn = new WebSocket(url);

            conn.onmessage = function(e) {
                var p = document.createElement("p");
                p.appendChild(document.createTextNode(e.data));
                output.appendChild(p);
            };

            form.addEventListener("submit", function(e){
                conn.send(input.value);
                input.value = "";
                e.preventDefault();
            });
        |]

main :: IO ()
main = do
    chan <- atomically newBroadcastTChan
    warp 3000 $ App chan
