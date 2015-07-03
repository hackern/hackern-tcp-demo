import Hans.Address.IP4
import Hans.Device.Xen
import Hans.DhcpClient
import Hans.Layer.Dns(DnsException)
import qualified Hans.NetworkStack as NS

import Network.Socket
import Network.Socket.Internal(setNetworkHansStack)

import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Maybe
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import System.IO

import Hypervisor.Debug
import Hypervisor.Console
import Hypervisor.XenStore
import XenDevice.NIC

initialNetworkStack :: XenStore -> IO (NS.NetworkStack, IP4)
initialNetworkStack xs = do
  nics <- listNICs xs
  let macstr = head nics
      mac = read macstr
  nic <- openNIC xs macstr
  ns <- NS.newNetworkStack
  NS.addDevice ns mac (xenSend nic) (xenReceiveLoop nic)
  NS.deviceUp ns mac
  
  res <- newEmptyMVar
  dhcpDiscover ns mac (putMVar res) -- This is neccessary.
  ip <- takeMVar res

  return (ns, ip)

main = do
  con <- initXenConsole
  xs <- initXenStore

  (ns, ip) <- initialNetworkStack xs

  addr:_ <- getAddrInfo (Just defaultHints) (Just $ show ip) (Just "9001")

  setNetworkHansStack ns
  sock <- socket (addrFamily addr) Stream defaultProtocol
  bind sock (addrAddress addr)
  listen sock 5
  writeConsole con $ "listen on " ++ fromJust (addrCanonName addr) ++ "\n"

  forever $ do
    (connsock, clientaddr) <- accept sock
    writeConsole con $ "connection from " ++ show clientaddr ++ "\n"
    str <- recv connsock 16
    writeConsole con $ str ++ "\n"
    _ <- send connsock "hello, world"
    close connsock
    threadDelay 100000
