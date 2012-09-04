{-# LANGUAGE RecordWildCards #-}
module Graphics.X11.XInput.Types where

#include <X11/Xlib.h>
#include <X11/extensions/XInput2.h>

import Control.Applicative
import Control.Monad
import qualified Data.Map as M
import Data.Bits
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Text.Printf
import qualified Graphics.X11 as X11
import qualified Graphics.X11.Xlib.Extras as E

instance Eq E.Event where
  x == y = E.ev_serial x == E.ev_serial y

genericEvent :: X11.EventType
genericEvent = 35

type Opcode = CInt

-- | Type for X11 cookie events
data EventCookie = EventCookie {
    ecEvent     :: E.Event   -- ^ Usual X11 event
  , ecExtension :: Opcode    -- ^ X11 extension identifier 
  , ecType      :: EventType -- ^ Event type
  , ecCookie    :: CUInt
  , ecData      :: Event     -- ^ XInput event
  }
  deriving (Eq)

instance Show EventCookie where
  show e = printf "<%s: %s>"
                  (show $ ecType e)
                  (show $ ecData e)

-- | XInput events
data Event = Event {
    eSendEvent :: Bool          -- ^ True if event was sent by SendEvent
  , eDisplay   :: X11.Display   -- 
  , eExtension :: Opcode        -- ^ X11 extension identifier
  , eType      :: EventType     -- ^ Event type
  , eDeviceId  :: DeviceID      -- ^ Device identifier
  , eSpecific  :: EventSpecific -- ^ Event-specific data
  }
  deriving (Eq)

instance Show Event where
  show e = printf "Event [device #%s] (%s)"
                  (show $ eDeviceId e)
                  (show $ eSpecific e)

-- | Event-specific info
data EventSpecific =
    GPointerEvent {
      peSourceId  :: CInt         -- ^ Source device identifier
    , peDetail    :: Int          -- ^ Detailed info; for example, button number or keycode
    , peRoot      :: X11.Window   -- ^ Root window
    , peEvent     :: X11.Window   -- ^ Event window
    , peChild     :: X11.Window   -- ^ Child window
    , peRootX     :: CDouble
    , peRootY     :: CDouble
    , peEventX    :: CDouble
    , peEventY    :: CDouble
    , peSpecific  :: PointerEvent -- ^ Event-specific
    }                             -- ^ General constructor for all pointer-related events
  | PropertyEvent {
      peProperty :: X11.Atom,
      peWhat :: CInt }
  | DeviceChangedEvent {
      dceReason :: CInt,
      dceClasses :: [GDeviceClass] }
  | UnsupportedEvent CInt
  deriving (Eq)

instance Show EventSpecific where
  show (GPointerEvent {..}) =
      printf "from %s [%s], at (%.2f, %.2f): %s"
             (show peSourceId)
             (show peDetail)
             (realToFrac peRootX :: Double)
             (realToFrac peRootY :: Double)
             (show peSpecific)

  show (PropertyEvent {..}) =
      printf "property %s: %s"
             (show peProperty)
             (show peWhat)

  show (DeviceChangedEvent {..}) =
      printf "device change [reason %s]: classes %s"
             (show dceReason)
             (show dceClasses)

  show (UnsupportedEvent e) = "unsupported event #" ++ show e

-- | All pointer-related event details
data PointerEvent =
    EnterLeaveEvent {
      eeMode       :: CInt
    , eeFocus      :: Bool
    , eeSameScreen :: Bool
    , peButtons    :: ButtonState
    , peMods       :: ModifierState
    , peGroup      :: GroupState
    }                               -- ^ XIEnterEvent or XILeaveEvent
  | DeviceEvent {
      deType :: EventType
    , deFlags :: CInt
    , peButtons :: ButtonState
    , deValuators :: ValuatorState
    , peMods :: ModifierState
    , peGroup :: GroupState
    }                               -- ^ Device event, such as button press
  deriving (Eq, Show)

-- | XInput event type
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

eventType2int :: Num a => EventType -> a
eventType2int et = fromIntegral $ fromEnum et + 1

int2eventType :: Integral a => a -> EventType
int2eventType n = toEnum (fromIntegral n - 1)

data EventMask = EventMask {
    emDeviceID :: DeviceID,
    emMask :: [Int] }
  deriving (Eq, Show)

instance Storable EventMask where
  sizeOf (EventMask dev mask) =
      let w = sizeOf (0 :: CInt)
      in (length mask + 2) * w

  alignment _ = alignment (0 :: CInt)

  peek ptr = do
      dev <- {# get XIEventMask->deviceid #} ptr
      len <- {# get XIEventMask->mask_len #} ptr
      maskPtr <- {# get XIEventMask->mask #} ptr
      mask <- peekArray (fromIntegral len) maskPtr
      return $ EventMask dev (map fromIntegral mask)

  poke ptr (EventMask dev mask) = do
      {# set XIEventMask->deviceid #} ptr dev
      let len = length mask
          w = sizeOf (0 :: CInt)
          p = sizeOf (nullPtr :: Ptr CUChar)
          maskPtr = castPtr (ptr `plusPtr` (w*2 + p)) :: Ptr CInt
      {# set XIEventMask->mask_len #} ptr (fromIntegral len)
      pokeArray maskPtr (map fromIntegral mask)

{# pointer *XIEventMask as EventMaskPtr -> EventMask #}

{# pointer *XGenericEventCookie as EventCookiePtr -> EventCookie #}

{# pointer *XIEvent as EventPtr -> Event #}

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

type DeviceID = CInt

-- | Device info
data DeviceInfo = DeviceInfo {
    diID         :: DeviceID       -- ^ Device identifier
  , diName       :: String         -- ^ Device name
  , diUse        :: DeviceType     -- ^ Device type: master or slave
  , diAttachment :: DeviceID       -- ^ Identifier of device this device is attached to
  , diEnabled    :: Bool           -- 
  , diClasses    :: [GDeviceClass] -- ^ Device classes
  }
  deriving (Eq)

instance Show DeviceInfo where
  show x = printf "<%s #%s: %s, attached to #%s, classes: %s>"
                  (show $ diUse x)
                  (show $ diID x)
                  (diName x)
                  (show $ diAttachment x)
                  (show $ diClasses x)

{# pointer *XIDeviceInfo as DeviceInfoPtr -> DeviceInfo #}

-- | Type of device class
data DeviceClassType =
    XIKeyClass
  | XIButtonClass
  | XIValuatorClass
  deriving (Eq, Show, Ord, Enum)

-- | Any device class
data GDeviceClass = GDeviceClass {
    dcType     :: DeviceClassType
  , dcSourceId :: Int
  , dcSpecific :: DeviceClass
  }
  deriving (Eq)

instance Show GDeviceClass where
  show t = printf "<%s [%s]>" (show $ dcType t) (show $ dcSpecific t)

{# pointer *XIAnyClassInfo as GDeviceClassPtr -> GDeviceClass #}

type Mask = [CUChar]

data ButtonState = ButtonState {
    bsMask :: [Int] }
  deriving (Eq, Show)

{# pointer *XIButtonState as ButtonStatePtr -> ButtonState #}

data ModifierState = ModifierState {
    msBase      :: Int
  , msLatched   :: Int
  , msLocked    :: Int
  , msEffective :: Int
  }
  deriving (Eq, Show)

{# pointer *XIModifierState as ModifierStatePtr -> ModifierState #}

type GroupState = ModifierState

type ValuatorState = M.Map Int Double

{# pointer *XIValuatorState as ValuatorStatePtr -> ValuatorState #}

-- | Device class specific info
data DeviceClass =
    ButtonClass {
      dcNumButtons :: Int
    , dcLabels     :: [X11.Atom]
    , dcState      :: ButtonState
    }
  | KeyClass {
      dcNumKeycodes :: Int
    , dcKeycodes    :: [Int]
    }
  | ValuatorClass {
      dcNumber     :: Int
    , dcLabel      :: X11.Atom
    , dcMin        :: Double
    , dcMax        :: Double
    , dcValue      :: Double
    , dcResolution :: Int
    , dcMode       :: Int
    }
  deriving (Eq)

instance Show DeviceClass where
  show (ButtonClass n _ _) = printf "%s buttons" (show n)
  show (KeyClass n _) = printf "%s keycodes" (show n)
  show (ValuatorClass _ _ min max _ _ _) =
      printf "%.2f..%.2f" min max

data SelectDevices =
    XIAllDevices
  | XIAllMasterDevices
  | OneDevice DeviceID
  deriving (Eq, Show, Ord)

data GrabModifiers = GrabModifiers {
  gModifiers :: Int,
  gStatus :: Int }
  deriving (Eq, Show)

instance Storable GrabModifiers where
  sizeOf x = 2 * sizeOf (0 :: CInt)
  alignment _ = alignment (0 :: CInt)

  peek ptr = GrabModifiers
      <$> (fromIntegral <$> {# get XIGrabModifiers->modifiers #} ptr)
      <*> (fromIntegral <$> {# get XIGrabModifiers->status #} ptr)
  
  poke ptr (GrabModifiers mods status) = do
    {# set XIGrabModifiers->modifiers #} ptr (fromIntegral mods)
    {# set XIGrabModifiers->status #} ptr (fromIntegral status)

{# pointer *XIGrabModifiers as GrabModifiersPtr -> GrabModifiers #}

selectDevices :: SelectDevices -> CInt
selectDevices XIAllDevices = 0
selectDevices XIAllMasterDevices = 1
selectDevices (OneDevice n) = n

ptr2display :: Ptr () -> X11.Display
ptr2display = X11.Display . castPtr

display2ptr :: X11.Display -> Ptr ()
display2ptr (X11.Display ptr) = castPtr ptr

toBool 0 = False
toBool 1 = True

fromBool True = 1
fromBool False = 0

-- | XInput initialization result
data XInputInitResult =
    NoXInput                -- ^ Extension is not supported at all.
  | VersionMismatch Int Int -- ^ XInput 2.0 is not supported, but other version is.
  | InitOK Opcode           -- ^ XInput 2.0 is supported, return xi_opcode.
  deriving (Eq, Show)

