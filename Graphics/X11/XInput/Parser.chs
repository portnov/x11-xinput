{-# LANGUAGE TypeFamilies #-}
{-| Functions of this module perform parsing of XInput events structures. -}
module Graphics.X11.XInput.Parser where

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

import Graphics.X11.XInput.Types

class Struct a where
  type Pointer a
  peekStruct :: Pointer a -> IO a

peekClasses :: Int -> Ptr a -> IO [GDeviceClass]
peekClasses n ptr = do
  let ptr' = castPtr ptr :: Ptr GDeviceClassPtr
  classesPtrs <- peekArray (fromIntegral n) ptr'
  forM classesPtrs (peekStruct . castPtr)

checkByte :: CUChar -> [Int]
checkByte x = [i | i <- [0..7], x .&. (1 `shiftL` i) /= 0]

parseMask :: Mask -> [Int]
parseMask [] = []
parseMask [x] = checkByte x
parseMask list =
    let x  = last list
        xs = init list
    in  map (+ 8) (checkByte x) ++ parseMask xs

peekMask :: (Ptr a -> IO CInt) -> (Ptr a -> IO (Ptr CUChar)) -> Ptr a -> IO [Int]
peekMask getLength getMask ptr = do
  len <- trace "maskLen" $ getLength ptr
  maskPtr <- trace "maskPtr" $ getMask ptr
  mask <- trace "mask" $ peekArray (fromIntegral len) maskPtr
  return $ parseMask mask

packMask :: [Int] -> X11.KeyMask
packMask list = foldr (.|.) 0 $ map (1 `shiftL`) list

instance Struct DeviceInfo where
  type Pointer DeviceInfo = DeviceInfoPtr

  peekStruct ptr = do
    id <- {# get XIDeviceInfo->deviceid #} ptr
    namePtr <- {# get XIDeviceInfo->name #} ptr
    name <- peekCString namePtr
    use <- int2deviceType <$> {# get XIDeviceInfo->use #} ptr
    att <- {# get XIDeviceInfo->attachment #} ptr
    on <- toBool <$> {# get XIDeviceInfo->enabled #} ptr
    ncls <- fromIntegral <$> {# get XIDeviceInfo->num_classes #} ptr
    clsptr <- {# get XIDeviceInfo->classes #} ptr
    classes <- peekClasses ncls clsptr
    return $ DeviceInfo id name use att on classes

instance Struct GDeviceClass where
  type Pointer GDeviceClass = GDeviceClassPtr

  peekStruct ptr = do
    tp <- (toEnum . fromIntegral) <$> {# get XIAnyClassInfo->type #} ptr
    src <- {# get XIAnyClassInfo->sourceid #} ptr
    cls <- case tp of
             XIButtonClass   -> peekButtonClass ptr
             XIKeyClass      -> peekKeyClass ptr
             XIValuatorClass -> peekValuatorClass ptr
    return $ GDeviceClass tp (fromIntegral src) cls

instance Struct ButtonState where
  type Pointer ButtonState = GDeviceClassPtr

  peekStruct ptr = ButtonState
    <$> peekMask ({# get XIButtonClassInfo->state.mask_len #})
                 ({# get XIButtonClassInfo->state.mask #})
                 ptr

peekButtonClass :: GDeviceClassPtr -> IO DeviceClass
peekButtonClass ptr = do
  n <- {# get XIButtonClassInfo->num_buttons #} ptr
  labelsPtr <- {# get XIButtonClassInfo->labels #} ptr
  labels <- peekArray (fromIntegral n) labelsPtr
  st <- peekStruct ptr
  return $ ButtonClass (fromIntegral n) (map fromIntegral labels) st

peekKeyClass :: GDeviceClassPtr -> IO DeviceClass
peekKeyClass ptr = do
  n <- {# get XIKeyClassInfo->num_keycodes #} ptr
  kptr <- {# get XIKeyClassInfo->keycodes #} ptr
  keycodes <- peekArray (fromIntegral n) kptr
  return $ KeyClass (fromIntegral n) (map fromIntegral keycodes)

peekValuatorClass :: GDeviceClassPtr -> IO DeviceClass
peekValuatorClass ptr = ValuatorClass 
  <$> (fromIntegral <$> {# get XIValuatorClassInfo->number #} ptr)
  <*> (fromIntegral <$> {# get XIValuatorClassInfo->label #} ptr)
  <*> (realToFrac <$> {# get XIValuatorClassInfo->min #} ptr)
  <*> (realToFrac <$> {# get XIValuatorClassInfo->max #} ptr)
  <*> (realToFrac <$> {# get XIValuatorClassInfo->value #} ptr)
  <*> (fromIntegral <$> {# get XIValuatorClassInfo->resolution #} ptr)
  <*> (fromIntegral <$> {# get XIValuatorClassInfo->mode #} ptr)

instance Struct Int where
  type Pointer Int = Ptr CInt
  peekStruct x = fromIntegral <$> peek x

get_event_type :: X11.XEventPtr -> IO X11.EventType
get_event_type ptr = fromIntegral <$> {# get XEvent->type #} ptr

get_event_extension :: X11.XEventPtr -> IO Opcode
get_event_extension ptr = {# get XGenericEvent->extension #} ptr

instance Struct EventCookie where
  type Pointer EventCookie = EventCookiePtr

  peekStruct xev = do
    ev     <- E.getEvent (castPtr xev)
    ext    <- {# get XGenericEventCookie->extension #} xev
    et     <- {# get XGenericEventCookie->evtype #} xev
    cookie <- {# get XGenericEventCookie->cookie #} xev
    dptr  <- {# get XGenericEventCookie->data #}   xev
    cdata <- peekStruct (castPtr dptr)

    return $ EventCookie {
               ecEvent  = ev,
               ecExtension = ext,
               ecType   = int2eventType et,
               ecCookie = cookie,
               ecData   = cdata }

getXGenericEventCookie :: X11.XEventPtr -> IO EventCookie
getXGenericEventCookie = peekStruct . castPtr

instance Struct Event where
  type Pointer Event = EventPtr

  peekStruct de = do 
    se <- toBool <$> {# get XIEvent->send_event #} de
    dpy <- ptr2display <$> {# get XIEvent->display #} de
    ext <- {# get XIDeviceEvent->extension #} de
    evt <- int2eventType <$> {# get XIEvent->evtype #} de
    dev <- {# get XIDeviceEvent->deviceid #}  de
    spec <- peekEventSpecific evt de
    return $ Event se dpy ext evt dev spec


peekEventSpecific XI_PropertyEvent e = PropertyEvent
  <$> (fromIntegral <$> {# get XIPropertyEvent->property #} e)
  <*> {# get XIPropertyEvent->what #} e

peekEventSpecific XI_DeviceChanged e = do
  reason <- {# get XIDeviceChangedEvent->reason #} e
  ncls   <- (fromIntegral <$> {# get XIDeviceChangedEvent->num_classes #} e)
  clsPtr <- {# get XIDeviceChangedEvent->classes #} e
  classes <- peekClasses ncls clsPtr
  return $ DeviceChangedEvent reason classes

peekEventSpecific t e = GPointerEvent
  <$> {# get XIDeviceEvent->sourceid #} e
  <*> (fromIntegral <$> {# get XIDeviceEvent->detail #} e)
  <*> (fromIntegral <$> {# get XIDeviceEvent->root #}   e)
  <*> (fromIntegral <$> {# get XIDeviceEvent->event #}  e)
  <*> (fromIntegral <$> {# get XIDeviceEvent->child #}  e)
  <*> {# get XIDeviceEvent->root_x #}  e
  <*> {# get XIDeviceEvent->root_y #}  e
  <*> {# get XIDeviceEvent->event_x #} e
  <*> {# get XIDeviceEvent->event_y #} e
  <*> trace "pointerEvent" (peekPointerEvent t e)

trace :: Show a => String -> IO a -> IO a
trace _ x = x
-- trace prefix action = do
--   result <- action
--   putStrLn $ prefix ++ ": " ++ show result
--   return result

peekPointerEvent XI_Enter e = EnterLeaveEvent
  <$> {# get XIEnterEvent->mode #} e
  <*> (toBool <$> {# get XIEnterEvent->focus #} e)
  <*> (toBool <$> {# get XIEnterEvent->same_screen #} e)
  <*> (ButtonState <$>
        (peekMask ({# get XIEnterEvent->buttons.mask_len #})
                  ({# get XIEnterEvent->buttons.mask #})
                  e ) )
  <*> (ModifierState
        <$> (fromIntegral <$> {# get XIEnterEvent->mods.base #}      e)
        <*> (fromIntegral <$> {# get XIEnterEvent->mods.latched #}   e)
        <*> (fromIntegral <$> {# get XIEnterEvent->mods.locked #}    e)
        <*> (fromIntegral <$> {# get XIEnterEvent->mods.effective #} e))
  <*> (ModifierState
        <$> (fromIntegral <$> {# get XIEnterEvent->group.base #}      e)
        <*> (fromIntegral <$> {# get XIEnterEvent->group.latched #}   e)
        <*> (fromIntegral <$> {# get XIEnterEvent->group.locked #}    e)
        <*> (fromIntegral <$> {# get XIEnterEvent->group.effective #} e))

peekPointerEvent XI_Leave e = peekPointerEvent XI_Enter e

peekPointerEvent XI_RawKeyPress e = peekRawEvent XI_RawKeyPress e

peekPointerEvent XI_RawKeyRelease e = peekRawEvent XI_RawKeyRelease e

peekPointerEvent t e = DeviceEvent
  <$> trace "event type" (return t)
  <*> trace "flags" ({# get XIDeviceEvent->flags #} e)
  <*> trace "buttons" (ButtonState <$>
        (peekMask ({# get XIDeviceEvent->buttons.mask_len #})
                  ({# get XIDeviceEvent->buttons.mask #})
                  e ) )
  <*> trace "valuators" (do
        mask <- trace "mask" $ peekMask ({# get XIDeviceEvent->valuators.mask_len #})
                         ({# get XIDeviceEvent->valuators.mask #})
                         e
        valuesPtr <- trace "valuesPtr" $ {# get XIDeviceEvent->valuators.values #} e
        values <- trace "values" $ peekArray (length mask) valuesPtr :: IO [CDouble]
        let values' = map realToFrac values :: [Double]
        return $ M.fromList $ zip mask values' )
  <*> trace "mods" (ModifierState
        <$> (fromIntegral <$> {# get XIDeviceEvent->mods.base #}      e)
        <*> (fromIntegral <$> {# get XIDeviceEvent->mods.latched #}   e)
        <*> (fromIntegral <$> {# get XIDeviceEvent->mods.locked #}    e)
        <*> (fromIntegral <$> {# get XIDeviceEvent->mods.effective #} e))
  <*> trace "group" (ModifierState
        <$> (fromIntegral <$> {# get XIDeviceEvent->group.base #}      e)
        <*> (fromIntegral <$> {# get XIDeviceEvent->group.latched #}   e)
        <*> (fromIntegral <$> {# get XIDeviceEvent->group.locked #}    e)
        <*> (fromIntegral <$> {# get XIDeviceEvent->group.effective #} e))


peekRawEvent t e = RawEvent
  <$> trace "event type" (return t)
  <*> trace "flags" ({# get XIRawEvent->flags #} e)
  <*> trace "valuators" (do
        mask <- trace "mask" $ peekMask ({# get XIRawEvent->valuators.mask_len #})
                         ({# get XIRawEvent->valuators.mask #})
                         e
        valuesPtr <- trace "valuesPtr" $ {# get XIRawEvent->valuators.values #} e
        values <- trace "values" $ peekArray (length mask) valuesPtr :: IO [CDouble]
        let values' = map realToFrac values :: [Double]
        return $ M.fromList $ zip mask values' )
