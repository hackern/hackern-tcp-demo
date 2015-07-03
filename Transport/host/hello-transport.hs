import Network.Transport
import Network.Transport.TCP (createTransport, defaultTCPParameters)

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.String

main :: IO ()
main = do
  serverAddr <- newEmptyMVar
  serverDone <- newEmptyMVar

  Right transport <- createTransport "10.0.2.15" "10080" defaultTCPParameters

  -- "Server"
  forkIO $ do
    Right endpoint <- newEndPoint transport
    putMVar serverAddr (address endpoint)

    forever $ do
      event <- receive endpoint
      case event of
        Received _ msg -> print msg >> putMVar serverDone ()
        _ -> return () -- ignore

  -- "Client"
  forkIO $ do
    Right endpoint <- newEndPoint transport
    Right conn     <- do addr <- readMVar serverAddr
                         connect endpoint addr ReliableOrdered defaultConnectHints
    send conn [fromString "Hello world"]
    return ()

  -- Wait for server to finish
  takeMVar serverDone
  putStrLn "done"
