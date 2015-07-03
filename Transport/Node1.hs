import Hans.Address.IP4
import Util

import Hypervisor.Debug
import Hypervisor.Console
import Hypervisor.XenStore

import Network.Transport
import Network.Transport.TCP (createTransport, defaultTCPParameters)

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import qualified Data.ByteString.Char8 as BSC

main :: IO ()
main = do
  xs <- initXenStore
  (ns, ip) <- initialNetworkStack xs

  Right transport <- createTransport (show ip) "9001" defaultTCPParameters

  -- "Server"
  Right endpoint <- newEndPoint transport
  writeDebugConsole $ show (address endpoint) ++ "\n"

  forkIO $ do
    Right conn <- connect endpoint (address endpoint)
                          ReliableOrdered defaultConnectHints
    void $ send conn [BSC.pack "hello, me"]

  forever $ do
    event <- receive endpoint
    case event of
      Received _ msg            ->
        writeDebugConsole $ BSC.unpack (head msg) ++ "\n"
      -- ConnectionOpened _ _ addr ->
      --   writeDebugConsole $ "connection from " ++ show addr ++ "\n"
      _                         -> return () -- ignore
