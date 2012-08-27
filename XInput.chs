
module XInput where

#include <X11/Xlib.h>
#include <X11/extensions/XInput2.h>

import Control.Applicative
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Graphics.X11

data EventCookie = EventCookie {
  ecExtension :: CInt,
  ecType      :: CInt,
  ecCookie    :: CUInt,
  ecData      :: Ptr () }

data DeviceEvent = DeviceEvent {
  deExtension :: CInt,
  deType      :: CInt,
  deDeviceId  :: CInt,
  deSourceId  :: CInt,
  deDetail    :: CInt,
  deRoot      :: Window,
  deEvent     :: Window,
  deChild     :: Window,
  deRootX     :: CDouble,
  deRootY     :: CDouble,
  deEventX    :: CDouble,
  deEventY    :: CDouble,
  deFlags     :: CInt }

{# pointer *XGenericEventCookie as EventCookiePtr -> EventCookie #}

{# pointer *XIDeviceEvent as DeviceEventPtr -> DeviceEvent #}

ptr2display :: Ptr () -> Display
ptr2display = Display . castPtr

display2ptr :: Display -> Ptr ()
display2ptr (Display ptr) = castPtr ptr

toBool 0 = False
toBool 1 = True

{# fun unsafe XGetEventData as getEventData {display2ptr `Display', castPtr `EventCookiePtr'} -> `Bool' #}

get_xcookie :: EventCookiePtr -> IO EventCookie
get_xcookie xev = do
  ext    <- {# get XGenericEventCookie->extension #} xev
  et     <- {# get XGenericEventCookie->evtype #} xev
  cookie <- {# get XGenericEventCookie->cookie #} xev
  cdata  <- {# get XGenericEventCookie->data #}   xev
  return $ EventCookie {
             ecExtension = ext,
             ecType   = et,
             ecCookie = cookie,
             ecData   = cdata }

getXGenericEventCookie :: XEventPtr -> IO EventCookie
getXGenericEventCookie = get_xcookie . castPtr

get_device_event :: DeviceEventPtr -> IO DeviceEvent
get_device_event de = DeviceEvent 
  <$> {# get XIDeviceEvent->extension #} de
  <*> {# get XIDeviceEvent->type #}      de
  <*> {# get XIDeviceEvent->deviceid #}  de
  <*> {# get XIDeviceEvent->sourceid #}  de
  <*> {# get XIDeviceEvent->detail #}    de
  <*> (fromIntegral <$> ({# get XIDeviceEvent->root #}  de))
  <*> (fromIntegral <$> ({# get XIDeviceEvent->event #} de))
  <*> (fromIntegral <$> ({# get XIDeviceEvent->child #} de))
  <*> {# get XIDeviceEvent->root_x #}    de
  <*> {# get XIDeviceEvent->root_y #}    de
  <*> {# get XIDeviceEvent->event_x #}   de
  <*> {# get XIDeviceEvent->event_y #}   de
  <*> {# get XIDeviceEvent->flags #}     de
