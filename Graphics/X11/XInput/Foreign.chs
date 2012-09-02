{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}
module Graphics.X11.XInput.Foreign
  (queryDevice,
   setEventMask,
   getEventData,
   freeEventData,
   xiQueryDevice,
   xQueryExtension,
   xinputVersion
  ) where

#include <X11/Xlib.h>
#include <X11/extensions/XInput2.h>

import Control.Monad
import Data.Bits
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import qualified Graphics.X11 as X11

import Graphics.X11.XInput.Types
import Graphics.X11.XInput.Parser

-- | Query list of devices
queryDevice :: X11.Display
            -> SelectDevices   -- ^ Which devices to list
            -> IO [DeviceInfo]
queryDevice dpy devs = do
  alloca $ \nptr -> do
    dptr <- xiQueryDevice dpy (selectDevices devs) nptr
    n <- peek nptr
    let sz = {# sizeof XIDeviceInfo #}
        offsets = take (fromIntegral n) [0, sz ..]
        dptrs = map (plusPtr dptr) offsets
    forM dptrs peekStruct

foreign import ccall "Foreign.chs.h XIQueryDevice"
  xiQueryDevice :: X11.Display -> CInt -> Ptr CInt -> IO DeviceInfoPtr

foreign import ccall "Foreign.chs.h XQueryExtension"
  xQueryExtension :: X11.Display -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

foreign import ccall "Foreign.chs.h XIQueryVersion"
  xinputVersion :: X11.Display -> Ptr CInt -> Ptr CInt -> IO CInt

{# fun unsafe XGetEventData as getEventData {display2ptr `X11.Display', castPtr `EventCookiePtr'} -> `Bool' #}

{# fun unsafe XFreeEventData as freeEventData {display2ptr `X11.Display', castPtr `EventCookiePtr'} -> `()' #}

{# fun unsafe XISelectEvents as selectEvents {display2ptr `X11.Display', fromIntegral `X11.Window', castPtr `EventMaskPtr', `Int'} -> `()' #}

addMask :: Ptr CUChar -> EventType -> IO ()
addMask ptr t = do
  let event = eventType2int t
      offset = fromIntegral $ event `shiftR` 3
      mask  = (1 `shiftL` (event .&. 7)) :: CUChar
  value <- peekByteOff ptr offset :: IO CUChar
  let newValue = value .|. mask
  pokeByteOff ptr offset newValue

-- | Select XInput events.
setEventMask :: X11.Display
             -> X11.Window
             -> [EventType] -- ^ List of events to listen
             -> IO ()
setEventMask dpy win list = do
  let len = (eventType2int XI_RawMotion + 7) `shiftR` 3
  allocaBytes (fromIntegral len) $ \(maskptr :: EventMaskPtr) -> do
    {# set XIEventMask.deviceid #} maskptr 1
    {# set XIEventMask.mask_len #} maskptr len
    allocaArray (fromIntegral len) $ \mask -> do
      forM list $ addMask mask
      {# set XIEventMask.mask #}     maskptr mask
      selectEvents dpy win maskptr 1

