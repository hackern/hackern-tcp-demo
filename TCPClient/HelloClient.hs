import Hans.Device.Xen
import Hans.DhcpClient
import Hans.Address.IP4
import qualified Hans.NetworkStack as NS

import Network.Socket
import Network.Socket.Internal(setNetworkHansStack)
import Control.Monad
import Control.Concurrent
import qualified Data.ByteString.Lazy.Char8 as BSC

import Hypervisor.Debug
import Hypervisor.Console
import Hypervisor.XenStore
import XenDevice.NIC

initialNetworkStack :: XenStore -> IO (NS.NetworkStack, IP4)
initialNetworkStack xs = do
  nics <- listNICs xs
  let macstr = head nics
      mac = read macstr
  writeDebugConsole "debug\n"
  nic <- openNIC xs macstr
  writeDebugConsole "debug\n"
  ns <- NS.newNetworkStack
  NS.addDevice ns mac (xenSend nic) (xenReceiveLoop nic)
  NS.deviceUp ns mac
  
  res <- newEmptyMVar
  dhcpDiscover ns mac (putMVar res) -- This is neccessary.
  ip <- takeMVar res

  return (ns, ip)

main = do
  xs <- initXenStore
  (ns, _) <- initialNetworkStack xs

{-
  let addr = IP4 10 0 2 16
  sock <- NS.connect ns addr 9001 Nothing
  sent <- NS.sendBytes sock (BSC.pack "hi, halvm")
  str <- NS.recvBytes sock 512
  writeDebugConsole (BSC.unpack str ++ "\n")
-}
  setNetworkHansStack ns

  addr:_ <- getAddrInfo (Just defaultHints) (Just "10.0.2.16") (Just "9001")
  sock <- socket (addrFamily addr) Stream defaultProtocol
  connect sock (addrAddress addr)
  send sock "hi, halvm"
  str <- recv sock 16
  writeDebugConsole (str ++ "\n")
