{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent  (forkIO, killThread)
import           Control.Exception
import           Control.Monad (forever, unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Network.HTTP.Client (newManager, parseRequest)
import qualified Network.HTTP.Client.Internal as Http
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Stream as WS

import Debug.Trace hiding (traceId)


main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  req <- parseRequest "GET https://echo.websocket.org/"
  withWsStremFromHttpConnection req manager $ \stream ->
    WS.runClientWithStream stream "echo.websocket.org" "/" WS.defaultConnectionOptions [] app


app :: WS.ClientApp ()
app conn = do
  putStrLn "Connected!"

  -- Fork a thread that writes WS data to stdout
  tid <- forkIO $ forever $ do
    msg <- WS.receiveData conn
    T.putStrLn msg

  -- Read from stdin and write to WS
  let loop = do
        line <- T.getLine
        unless (T.null line) $ WS.sendTextData conn line >> loop

  loop
  WS.sendClose conn ("Bye!" :: Text)
  killThread tid


withWsStremFromHttpConnection :: Http.Request -> Http.Manager -> (WS.Stream -> IO a) -> IO a
withWsStremFromHttpConnection req manager action =
  Http.withProxiedConnection req manager $ \conn -> do
    bracket
      ( do
        let read = do
              traceM $ "Stream: BEGAN reading"
              bs <- Http.connectionRead $ conn
              traceM $ "Stream: FINISHED reading " ++ show bs
              return $
                if BS.null bs
                  then Nothing
                  else Just bs

            write =
              maybe
                (Http.connectionClose conn)
                (Http.connectionWrite conn . traceId "Stream: BEGAN writing" . BSL.toStrict)
        WS.makeStream read write
      )
      WS.close
      action

traceIdVia :: Show b => (a -> b) -> String -> a -> a
traceIdVia via prefix x = trace (prefix ++ ": " ++ show (via x)) x

traceId :: Show a => String -> a -> a
traceId = traceIdVia id
