
module XInput where

#include <X11/Xlib.h>
#include <X11/extensions/XInput2.h>

import Control.Applicative
import Control.Monad
import Data.Bits
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import qualified Graphics.X11 as X11
import Graphics.X11.Xlib.Extras

genericEvent :: X11.EventType
genericEvent = 35

type Opcode = CInt

data EventCookie = EventCookie {
  ecExtension :: CInt,
  ecType      :: EventType,
  ecCookie    :: CUInt,
  ecData      :: Ptr () }

data DeviceEvent = DeviceEvent {
  deExtension :: CInt,
  deType      :: CInt,
  deDeviceId  :: CInt,
  deSourceId  :: CInt,
  deDetail    :: CInt,
  deRoot      :: X11.Window,
  deEvent     :: X11.Window,
  deChild     :: X11.Window,
  deRootX     :: CDouble,
  deRootY     :: CDouble,
  deEventX    :: CDouble,
  deEventY    :: CDouble,
  deFlags     :: CInt }

data EventType =
    XI_DeviceChanged      --         1
  | XI_KeyPress           --         2
  | XI_KeyRelease         --         3
  | XI_ButtonPress        --         4
  | XI_ButtonRelease      --         5
  | XI_Motion             --         6
  | XI_Enter              --         7
  | XI_Leave              --         8
  | XI_FocusIn            --         9
  | XI_FocusOut           --         10
  | XI_HierarchyChanged   --         11
  | XI_PropertyEvent      --         12
  | XI_RawKeyPress        --         13
  | XI_RawKeyRelease      --         14
  | XI_RawButtonPress     --         15
  | XI_RawButtonRelease   --         16
  | XI_RawMotion          --         17
  deriving (Eq, Show, Ord, Enum)

eventType2int :: EventType -> CInt
eventType2int et = fromIntegral $ fromEnum et + 1

int2eventType :: CInt -> EventType
int2eventType n = toEnum (fromIntegral n - 1)

data EventMask =
    XI_DeviceChangedMask    --       (1 << XI_DeviceChanged)
  | XI_KeyPressMask         --       (1 << XI_KeyPress)
  | XI_KeyReleaseMask       --       (1 << XI_KeyRelease)
  | XI_ButtonPressMask      --       (1 << XI_ButtonPress)
  | XI_ButtonReleaseMask    --       (1 << XI_ButtonRelease)
  | XI_MotionMask           --       (1 << XI_Motion)
  | XI_EnterMask            --       (1 << XI_Enter)
  | XI_LeaveMask            --       (1 << XI_Leave)
  | XI_FocusInMask          --       (1 << XI_FocusIn)
  | XI_FocusOutMask         --       (1 << XI_FocusOut)
  | XI_HierarchyChangedMask --       (1 << XI_HierarchyChanged)
  | XI_PropertyEventMask    --       (1 << XI_PropertyEvent)
  | XI_RawKeyPressMask      --       (1 << XI_RawKeyPress)
  | XI_RawKeyReleaseMask    --       (1 << XI_RawKeyRelease)
  | XI_RawButtonPressMask   --       (1 << XI_RawButtonPress)
  | XI_RawButtonReleaseMask --       (1 << XI_RawButtonRelease)
  | XI_RawMotionMask        --       (1 << XI_RawMotion)
  deriving (Eq, Show, Ord, Enum)

eventMask2int :: EventMask -> CInt
eventMask2int em = 1 `shiftL` (fromEnum em + 1)

{# pointer *XGenericEventCookie as EventCookiePtr -> EventCookie #}

{# pointer *XIDeviceEvent as DeviceEventPtr -> DeviceEvent #}

data DeviceType =
    XIMasterPointer   --                    1
  | XIMasterKeyboard  --                    2
  | XISlavePointer    --                    3
  | XISlaveKeyboard   --                    4
  | XIFloatingSlave   --                    5
  deriving (Eq, Show, Ord, Enum)

deviceType2int :: DeviceType -> CInt
deviceType2int dt = fromIntegral (fromEnum dt + 1)

int2deviceType :: CInt -> DeviceType
int2deviceType n = toEnum (fromIntegral n - 1)

data DeviceInfo = DeviceInfo {
  diID :: Int,
  diName :: String,
  diUse :: DeviceType,
  diAttachment :: Int,
  diEnabled :: Bool,
  diNumClasses :: Int,
  dcClasses :: [GDeviceClass]}
  deriving (Eq, Show)

{# pointer *XIDeviceInfo as DeviceInfoPtr -> DeviceInfo #}

data DeviceClassType =
    XIKeyClass
  | XIButtonClass
  | XIValuatorClass
  deriving (Eq, Show, Ord, Enum)

data GDeviceClass = GDeviceClass {
  dcType :: DeviceClassType,
  dcSourceId :: Int,
  dcSpecific :: DeviceClass }
  deriving (Eq, Show)

{# pointer *XIAnyClassInfo as GDeviceClassPtr -> GDeviceClass #}

data ButtonState = ButtonState {
    bsMaskLen :: Int,
    bsMask :: String }
  deriving (Eq, Show)

{# pointer *XIButtonState as ButtonStatePtr -> ButtonState #}

data DeviceClass =
    ButtonClass {
      dcNumButtons :: Int,
      dcLabels :: [X11.Atom],
      dcState :: ButtonState }
  | KeyClass {
      dcNumKeycodes :: Int,
      dcKeycodes :: [Int] }
  | ValuatorClass {
      dcNumber :: Int,
      dcLabel :: X11.Atom,
      dcMin :: Double,
      dcMax :: Double,
      dcValue :: Double,
      dcResolution :: Int,
      dcMode :: Int }
  deriving (Eq, Show)

peekDeviceInfo :: DeviceInfoPtr -> IO DeviceInfo
peekDeviceInfo ptr = do
  id <- fromIntegral <$> {# get XIDeviceInfo->deviceid #} ptr
  namePtr <- {# get XIDeviceInfo->name #} ptr
  name <- peekCString namePtr
  use <- int2deviceType <$> {# get XIDeviceInfo->use #} ptr
  att <- fromIntegral <$> {# get XIDeviceInfo->attachment #} ptr
  on <- toBool <$> {# get XIDeviceInfo->enabled #} ptr
  ncls <- fromIntegral <$> {# get XIDeviceInfo->num_classes #} ptr
  clsptr <- {# get XIDeviceInfo->classes #} ptr
  classesPtrs <- peekArray ncls clsptr
  classes <- forM classesPtrs (peekDeviceClass)
  return $ DeviceInfo id name use att on ncls classes

peekDeviceClass :: GDeviceClassPtr -> IO GDeviceClass
peekDeviceClass ptr = do
  tp <- (toEnum . fromIntegral) <$> {# get XIAnyClassInfo->type #} ptr
  src <- {# get XIAnyClassInfo->sourceid #} ptr
  cls <- case tp of
           XIButtonClass   -> peekButtonClass ptr
           XIKeyClass      -> peekKeyClass ptr
           XIValuatorClass -> peekValuatorClass ptr
  return $ GDeviceClass tp (fromIntegral src) cls

peekButtonClass :: GDeviceClassPtr -> IO DeviceClass
peekButtonClass ptr = do
  n <- {# get XIButtonClassInfo->num_buttons #} ptr
  labelsPtr <- {# get XIButtonClassInfo->labels #} ptr
  labels <- peekArray (fromIntegral n) labelsPtr
  st <- peekButtonState ptr
  return $ ButtonClass (fromIntegral n) (map fromIntegral labels) st

peekButtonState :: GDeviceClassPtr -> IO ButtonState
peekButtonState ptr = do
  n <- {# get XIButtonClassInfo->state.mask_len #} ptr
  maskPtr <- {# get XIButtonClassInfo->state.mask #} ptr
  mask <- peekCStringLen (castPtr maskPtr, fromIntegral n)
  return $ ButtonState (fromIntegral n) mask

peekKeyClass :: GDeviceClassPtr -> IO DeviceClass
peekKeyClass ptr = do
  n <- {# get XIKeyClassInfo->num_keycodes #} ptr
  kptr <- {# get XIKeyClassInfo->keycodes #} ptr
  keycodes <- peekArray (fromIntegral n) kptr
  return $ KeyClass (fromIntegral n) (map fromIntegral keycodes)

peekValuatorClass = undefined

ptr2display :: Ptr () -> X11.Display
ptr2display = X11.Display . castPtr

display2ptr :: X11.Display -> Ptr ()
display2ptr (X11.Display ptr) = castPtr ptr

toBool 0 = False
toBool 1 = True

peekInt x = fromIntegral <$> peek x

foreign import ccall "Xinput.chs.h XQueryExtension"
  xQueryExtension :: X11.Display -> CString -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CInt

foreign import ccall "XInput.chs.h XIQueryVersion"
  xinputVersion :: X11.Display -> Ptr CInt -> Ptr CInt -> IO CInt

xinputMajor, xinputMinor :: CInt
xinputMajor = 2
xinputMinor = 0

-- | Returns Nothing if XInput 2.0 is supported, or
-- Just (major, minor) if another version is supported
xinputCheckVersion :: X11.Display -> IO (Maybe (Int, Int))
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

{# fun unsafe XGetEventData as getEventData {display2ptr `X11.Display', castPtr `EventCookiePtr'} -> `Bool' #}

{# fun unsafe XFreeEventData as freeEventData {display2ptr `X11.Display', castPtr `EventCookiePtr'} -> `()' #}

-- | XInput initialization result
data XInputInitResult =
    NoXInput                -- ^ Extension is not supported at all.
  | VersionMismatch Int Int -- ^ XInput 2.0 is not supported, but other version is.
  | InitOK Opcode           -- ^ XInput 2.0 is supported, return xi_opcode.
  deriving (Eq, Show)

-- | Initialize XInput 2.0 extension.
xinputInit :: X11.Display -> IO XInputInitResult
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
                   Nothing  -> return $ InitOK xi_opcode
                   Just (major, minor) -> return $ VersionMismatch major minor
            else return NoXInput

get_event_type :: X11.XEventPtr -> IO X11.EventType
get_event_type ptr = fromIntegral <$> {# get XEvent->type #} ptr

get_event_extension :: X11.XEventPtr -> IO CInt
get_event_extension ptr = {# get XGenericEvent->extension #} ptr

get_xcookie :: EventCookiePtr -> IO EventCookie
get_xcookie xev = do
  ext    <- {# get XGenericEventCookie->extension #} xev
  et     <- {# get XGenericEventCookie->evtype #} xev
  cookie <- {# get XGenericEventCookie->cookie #} xev
  cdata  <- {# get XGenericEventCookie->data #}   xev
  return $ EventCookie {
             ecExtension = ext,
             ecType   = int2eventType et,
             ecCookie = cookie,
             ecData   = cdata }

getXGenericEventCookie :: X11.XEventPtr -> IO EventCookie
getXGenericEventCookie = get_xcookie . castPtr

handleXCookie :: X11.Display -> CInt -> X11.XEventPtr -> (Event -> IO a) -> (EventCookie -> IO a) -> IO a
handleXCookie dpy xi_opcode xev evHandler cookieHandler = do
  evtype <- get_event_type xev
  ext    <- get_event_extension xev
  hasCookie <- getEventData dpy (castPtr xev)
  result <- if (evtype == genericEvent) && (ext == xi_opcode) && hasCookie
              then cookieHandler =<< getXGenericEventCookie xev
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

