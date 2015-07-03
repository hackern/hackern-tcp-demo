import Hans.Address.IP4
import Util

import Hypervisor.Debug
import Hypervisor.XenStore

import Network.Transport
import Network.Transport.TCP (createTransport, defaultTCPParameters)

import Control.Monad
import Control.Concurrent
import Control.Concurrent.QSemN
import Data.ByteString.Char8 as BSC

main :: IO ()
main = do
  xs <- initXenStore
  (ns, ip) <- initialNetworkStack xs

  Right transport <- createTransport (show ip) "9001" defaultTCPParameters
  Right endpoint <- newEndPoint transport

  sem <- newQSemN 0
  forkIO . forever $ do
    event <- receive endpoint
    case event of
      ConnectionOpened _ _ addr ->
        writeDebugConsole $ "connection from " ++ show addr ++ "\n"
      Received _ bss -> do 
        forM bss $ \bs ->
          writeDebugConsole $ (BSC.unpack bs) ++ "\n"
        signalQSemN sem 1

  Right conn <- connect endpoint (address endpoint)
                        ReliableOrdered defaultConnectHints
  forM_ [0..9] $ \i ->
    send conn [BSC.pack (show (2*i)), BSC.pack (show (2*i + 1))]

  waitQSemN sem 10 
