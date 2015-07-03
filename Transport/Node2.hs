import Hans.Address.IP4
import Util

import Hypervisor.Debug
import Hypervisor.Console
import Hypervisor.XenStore

import Network.Transport
import Network.Transport.TCP (createTransport, defaultTCPParameters,
                              encodeEndPointAddress)

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import qualified Data.ByteString.Char8 as BSC


main :: IO ()
main = do
  xs <- initXenStore
  (ns, ip) <- initialNetworkStack xs

  Right transport <- createTransport (show ip) "9001" defaultTCPParameters

  -- "Client"
  Right endpoint <- newEndPoint transport
  writeDebugConsole $ show (address endpoint) ++ "\n"
  
  let addr = encodeEndPointAddress "10.0.2.16" "9001" 0
  Right conn <- connect endpoint addr ReliableOrdered defaultConnectHints
  void $ send conn [BSC.pack "hello, world"]
  threadDelay 1000000
