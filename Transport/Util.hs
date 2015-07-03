module Util where

import Hypervisor.XenStore
import XenDevice.NIC

import qualified Hans.NetworkStack as NS
import Hans.Address.IP4
import Hans.Device.Xen
import Hans.DhcpClient
import Hans.Layer.Dns(DnsException)

import Network.Socket.Internal(setNetworkHansStack)

import Control.Concurrent.MVar

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
  ip  <- takeMVar res
  setNetworkHansStack ns -- This is also neccessary.

  return (ns, ip)
