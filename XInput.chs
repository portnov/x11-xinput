
module XInput where

#include <X11/Xlib.h>
#include <X11/extensions/XInput2.h>

import Control.Applicative
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Graphics.X11
import Graphics.X11.Xlib.Extras

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

peekInt x = fromIntegral <$> peek x

foreign import ccall "Xinput.chs.h XQueryExtension"
  xQueryExtension :: Display -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

foreign import ccall "XInput.chs.h XIQueryVersion"
  xinputVersion :: Display -> Ptr CInt -> Ptr CInt -> IO CInt

xinputMajor, xinputMinor :: CInt
xinputMajor = 2
xinputMinor = 0

-- | Returns Nothing if XInput 2.0 is supported, or
-- Just (major, minor) if another version is supported
xinputCheckVersion :: Display -> IO (Maybe (Int, Int))
xinputCheckVersion dpy = do
  alloca $ \majorPtr ->
    alloca $ \minorPtr -> do
      poke majorPtr xinputMajor
      poke minorPtr xinputMinor
      result <- xinputVersion dpy majorPtr minorPtr
      supportedMajor <- peek majorPtr
      supportedMinor <- peek minorPtr
      if result == 1
        then return $ Just (fromIntegral supportedMajor, fromIntegral supportedMinor)
        else return Nothing

{# fun unsafe XGetEventData as getEventData {display2ptr `Display', castPtr `EventCookiePtr'} -> `Bool' #}

{# fun unsafe XFreeEventData as freeEventData {display2ptr `Display', castPtr `EventCookiePtr'} -> `()' #}

data XInputInitResult =
    NoXInput
  | VersionMismatch Int Int
  | InitOK Int
  deriving (Eq, Show)

xinputInit :: Display -> IO XInputInitResult
xinputInit dpy = do
  withCString "XInputExtension" $ \xinput ->
    alloca $ \opcode -> 
      alloca $ \event ->
        alloca $ \error -> do
          result <- xQueryExtension dpy xinput opcode event error
          if result /= 0
            then do
                 xi_opcode <- peek opcode
                 mbVer <- xinputCheckVersion dpy
                 case mbVer of
                   Nothing  -> return $ InitOK (fromIntegral xi_opcode)
                   Just (major, minor) -> return $ VersionMismatch major minor
            else return NoXInput

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

handleXCookie :: Display -> XEventPtr -> (Event -> IO a) -> (Ptr () -> IO a) -> IO a
handleXCookie dpy xev evHandler cookieHandler = do
  hasCookie <- getEventData dpy (castPtr xev)
  result <- if hasCookie
              then do
                   cookie <- getXGenericEventCookie xev
                   cookieHandler (ecData cookie)
              else evHandler =<< getEvent xev
  freeEventData dpy (castPtr xev)
  return result

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

