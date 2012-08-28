{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.X11.XInput.Foreign where

#include <X11/Xlib.h>
#include <X11/extensions/XInput2.h>

import Control.Monad
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import qualified Graphics.X11 as X11

import Graphics.X11.XInput.Types
import Graphics.X11.XInput.Parser

queryDevice :: X11.Display -> SelectDevices -> IO [DeviceInfo]
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

