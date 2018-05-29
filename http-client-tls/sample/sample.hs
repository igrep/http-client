{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent  (forkIO)
import           Control.Exception
import           Control.Monad (forever, unless, void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.KeyedPool (managedResource)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Network.HTTP.Client (defaultManagerSettings, newManager, parseRequest, httpLbs)
import qualified Network.HTTP.Client.Internal as Http
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Stream as WS

import Debug.Trace hiding (traceId)


main :: IO ()
-- main = httpMain
main = wsMain


httpMain :: IO ()
httpMain = do
  man <- newManager defaultManagerSettings
  let req = "http://httpbin.org"
  void $ httpLbs req man


wsMain :: IO ()
wsMain = do
  manager <- newManager tlsManagerSettings
  req <- Http.mSetProxy manager <$> parseRequest "GET https://echo.websocket.org/"
  withWsStremFromHttpConnection req manager $ \stream ->
    WS.runClientWithStream stream "echo.websocket.org" "/" WS.defaultConnectionOptions [] app


app :: WS.ClientApp ()
app conn = do
  putStrLn "Connected!"

  -- Fork a thread that writes WS data to stdout
  _ <- forkIO $ forever $ do
    msg <- WS.receiveData conn
    T.putStrLn msg

  -- Read from stdin and write to WS
  let loop = do
        line <- T.getLine
        unless (T.null line) $ WS.sendTextData conn line >> loop

  loop
  WS.sendClose conn ("Bye!" :: Text)


-- Managed に囲われた状態の Connection を直接扱う APIを提供する？
-- withProxiedConnection
withWsStremFromHttpConnection :: Http.Request -> Http.Manager -> (WS.Stream -> IO a) -> IO a
withWsStremFromHttpConnection req manager action = do
  mHttpConn <- Http.getConn req manager
  bracket
    ( do
      let read = do
            traceM $ "Stream: BEGAN reading"
            bs <- Http.connectionRead $ managedResource mHttpConn
            traceM $ "Stream: FINISHED reading " ++ show bs
            return $
              if BS.null bs
                then Nothing
                else Just bs

          write =
            maybe
              (Http.connectionClose $ managedResource mHttpConn)
              (Http.connectionWrite (managedResource mHttpConn) . traceId "Stream: BEGAN writing" . BSL.toStrict)
      WS.makeStream read write
    )
    ( \stream ->
      WS.close stream `catch` ((\_ -> return ()) :: SomeException -> IO ())
    )
    action

traceIdVia :: Show b => (a -> b) -> String -> a -> a
traceIdVia via prefix x = trace (prefix ++ ": " ++ show (via x)) x

traceId :: Show a => String -> a -> a
traceId = traceIdVia id
