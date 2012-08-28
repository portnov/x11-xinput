
module Graphics.X11.XInput.Internal where

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

queryDevice :: X11.Display -> SelectDevices -> IO [DeviceInfo]
queryDevice dpy devs = do
  alloca $ \nptr -> do
    dptr <- xiQueryDevice dpy (selectDevices devs) nptr
    n <- peek nptr
    let sz = {# sizeof XIDeviceInfo #}
        offsets = take (fromIntegral n) [0, sz ..]
        dptrs = map (plusPtr dptr) offsets
    forM dptrs peekDeviceInfo

