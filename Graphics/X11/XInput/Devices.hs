
module Graphics.X11.XInput.Devices
  (Device (..),
   DevicesMap,
   buildDevicesMap,
   showDevicesMap
  ) where

import qualified Data.Map as M
import Text.Printf

import Graphics.X11.XInput.Types

data Device = Device {
  masterDevice :: DeviceInfo,
  slaveDevices :: [DeviceInfo] }
  deriving (Eq, Show)

type DevicesMap = M.Map DeviceID Device

buildDevicesMap :: [DeviceInfo] -> DevicesMap
buildDevicesMap list = M.fromList $ build [] list
  where
    build :: [(DeviceID, Device)] -> [DeviceInfo] -> [(DeviceID, Device)]
    build done [] = done
    build done (d:ds) = 
      case diUse d of
        XIMasterPointer  -> build (simply d: done) ds
        XIMasterKeyboard -> build (simply d: done) ds
        XISlavePointer   -> build (up d done) ds
        XISlaveKeyboard  -> build (up d done) ds
        XIFloatingSlave  -> build (up d done) ds

    simply d = (diID d, Device d [])

    up :: DeviceInfo -> [(DeviceID, Device)] -> [(DeviceID, Device)]
    up d ds = map (update d) ds

    update :: DeviceInfo -> (DeviceID, Device) -> (DeviceID, Device)
    update slave pair@(masterID, master)
      | diAttachment slave == masterID = (masterID, add master slave)
      | otherwise = pair

    add master slave =
        master {slaveDevices = slave: slaveDevices master}

showDevicesMap :: DevicesMap -> String
showDevicesMap m = unlines $ map go $ M.assocs m
  where
    go (devID, dev) =
           (printf "#%s: %s\n" (show devID) (show $ masterDevice dev))
        ++ (unlines $ map one $ slaveDevices dev)

    one dev = printf " +-- #%s: %s"
                     (show $ diID dev)
                     (show dev)
