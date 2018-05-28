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


-- TLSにした結果、またしても書き込みをする前にGCされてcloseしてしまっているように見える。
-- おそらく、TLSでの接続については `convertConnection` で行っているとおり、
-- `Network.Connection.Connection` をラップした `Network.HTTP.Client.Connection` になっているためと思われる。
-- どうやって回避するべきものか。。。
wsMain :: IO ()
wsMain = do
  manager <- newManager tlsManagerSettings
  req <- Http.mSetProxy manager <$> parseRequest "GET https://echo.websocket.org/"
  withWsStremFromHttpConnection (managedResource <$> Http.getConn req manager) $ \stream ->
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


withWsStremFromHttpConnection :: (IO Http.Connection) -> (WS.Stream -> IO a) -> IO a
withWsStremFromHttpConnection getHttpConn action =
  bracket
    ( do
      httpConn <- getHttpConn
      let read = do
            traceM $ "Stream: BEGAN reading"
            bs <- Http.connectionRead httpConn
            traceM $ "Stream: FINISHED reading " ++ show bs
            return $
              if BS.null bs
                then Nothing
                else Just bs

          write =
            maybe
              (Http.connectionClose httpConn)
              (Http.connectionWrite httpConn . traceId "Stream: BEGAN writing" . BSL.toStrict)
      WS.makeStream read write 
    )
    (WS.close)
    action

traceIdVia :: Show b => (a -> b) -> String -> a -> a
traceIdVia via prefix x = trace (prefix ++ ": " ++ show (via x)) x

traceId :: Show a => String -> a -> a
traceId = traceIdVia id
