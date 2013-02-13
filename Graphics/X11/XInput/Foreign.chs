{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}
module Graphics.X11.XInput.Foreign
  (queryDevice,
   setEventMask,
   getEventData,
   freeEventData,
   xiQueryDevice,
   xQueryExtension,
   xinputVersion,
   grabDevice, ungrabDevice,
   grabButton, ungrabButton,
   grabKeycode, ungrabKeycode
  ) where

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

{# fun unsafe XGetEventData as getEventData {display2ptr `X11.Display',
                                             castPtr `EventCookiePtr'} -> `Bool' #}

{# fun unsafe XFreeEventData as freeEventData {display2ptr `X11.Display',
                                               castPtr `EventCookiePtr'} -> `()' #}

pokeAlloca :: Storable a => a -> (Ptr a -> IO b) -> IO b
pokeAlloca x f =
  alloca $ \ptr -> do
    poke ptr x
    f ptr

{# fun unsafe XISelectEvents as selectEvents {display2ptr `X11.Display',
                                              fromIntegral `X11.Window',
                                              castPtr `EventMaskPtr',
                                              `Int'} -> `()' #}

foreign import ccall unsafe "Foreign.chs.h XIGrabDevice"
    xiGrabDevice :: X11.Display
                 -> DeviceID
                 -> X11.Window
                 -> X11.Time
                 -> X11.Cursor
                 -> X11.GrabMode
                 -> X11.GrabMode
                 -> CInt
                 -> EventMaskPtr
                 -> IO CInt

foreign import ccall unsafe "Foreign.chs.h XIUngrabDevice"
    xiUngrabDevice :: X11.Display
                   -> DeviceID
                   -> X11.Time
                   -> IO X11.Status

foreign import ccall unsafe "Foreign.chs.h XIGrabButton"
    xiGrabButton :: X11.Display
                 -> DeviceID
                 -> CInt
                 -> X11.Window
                 -> X11.Cursor
                 -> X11.GrabMode
                 -> X11.GrabMode
                 -> CInt
                 -> EventMaskPtr
                 -> CInt
                 -> GrabModifiersPtr
                 -> IO CInt

foreign import ccall unsafe "Foreign.chs.h XIUngrabButton"
  xiUngrabButton :: X11.Display
                 -> DeviceID
                 -> X11.Button
                 -> X11.Window
                 -> CInt
                 -> GrabModifiersPtr
                 -> IO X11.Status

foreign import ccall unsafe "Foreign.chs.h XIGrabKeycode"
  xiGrabKeycode :: X11.Display
                -> DeviceID
                -> X11.KeyCode
                -> X11.Window
                -> X11.GrabMode
                -> X11.GrabMode
                -> CInt
                -> EventMaskPtr
                -> CInt
                -> GrabModifiersPtr
                -> IO CInt

foreign import ccall unsafe "Foreign.chs.h XIUngrabKeycode"
  xiUngrabKeycode :: X11.Display
                 -> DeviceID
                 -> X11.KeyCode
                 -> X11.Window
                 -> CInt
                 -> GrabModifiersPtr
                 -> IO X11.Status

grabDevice :: X11.Display
           -> DeviceID
           -> X11.Window
           -> X11.Cursor
           -> X11.GrabMode
           -> X11.GrabMode
           -> Bool
           -> EventMask 
           -> IO X11.Status
grabDevice dpy dev win cursor
           grab_mode paired_mode owner_events mask =
  pokeAlloca mask $ \maskPtr ->
    xiGrabDevice dpy dev win 0 cursor
                 grab_mode paired_mode
                 (fromBool owner_events)
                 maskPtr

ungrabDevice :: X11.Display
             -> DeviceID
             -> IO X11.Status
ungrabDevice dpy dev =
   xiUngrabDevice dpy dev 0

grabButton :: X11.Display
           -> SelectDevices
           -> X11.Button
           -> X11.Window
           -> X11.Cursor
           -> X11.GrabMode
           -> X11.GrabMode
           -> Bool
           -> [EventType]
           -> [GrabModifiers]
           -> IO [GrabModifiers]
grabButton dpy dev btn win cursor
           grab_mode paired_mode owner_events
           events mods = do
  let nMods = length mods
  allocaArray nMods $ \modsPtr ->
    withEventMask dpy win events $ \maskPtr -> do
      pokeArray modsPtr mods
      n <- xiGrabButton dpy (selectDevices dev)
                        (fromIntegral btn) win cursor
                        grab_mode paired_mode
                        (fromBool owner_events)
                        maskPtr
                        (fromIntegral nMods)
                        modsPtr
      peekArray (fromIntegral n) modsPtr

ungrabButton :: X11.Display
             -> SelectDevices
             -> X11.Button
             -> X11.Window
             -> [GrabModifiers]
             -> IO X11.Status
ungrabButton dpy dev btn win mods = do
  let nMods = length mods
  allocaArray nMods $ \modsPtr -> do
    xiUngrabButton dpy (selectDevices dev)
                   (fromIntegral btn) win
                   (fromIntegral nMods)
                   modsPtr

grabKeycode  :: X11.Display
             -> SelectDevices
             -> X11.KeyCode
             -> X11.Window
             -> X11.GrabMode
             -> X11.GrabMode
             -> Bool
             -> [EventType]
             -> [GrabModifiers]
             -> IO [GrabModifiers]
grabKeycode dpy dev btn win
            grab_mode paired_mode owner_events
            events mods = do
  let nMods = length mods
  allocaArray nMods $ \modsPtr ->
    withEventMask dpy win events $ \maskPtr -> do
      pokeArray modsPtr mods
      n <- xiGrabKeycode dpy (selectDevices dev)
                         (fromIntegral btn) win
                         grab_mode paired_mode
                         (fromBool owner_events)
                         maskPtr
                         (fromIntegral nMods)
                         modsPtr
      peekArray (fromIntegral n) modsPtr

ungrabKeycode :: X11.Display
             -> SelectDevices
             -> X11.KeyCode
             -> X11.Window
             -> [GrabModifiers]
             -> IO X11.Status
ungrabKeycode dpy dev btn win mods = do
  let nMods = length mods
  allocaArray nMods $ \modsPtr -> do
    xiUngrabKeycode dpy (selectDevices dev)
                    (fromIntegral btn) win
                    (fromIntegral nMods)
                    modsPtr

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
setEventMask dpy win list =
  withEventMask dpy win list $ \maskptr ->
      selectEvents dpy win maskptr 1

withEventMask :: X11.Display
              -> X11.Window
              -> [EventType]
              -> (EventMaskPtr -> IO a)
              -> IO a
withEventMask dpy win list callback = do
  let len = (eventType2int XI_RawMotion + 7) `shiftR` 3
  allocaBytes (fromIntegral len) $ \(maskptr :: EventMaskPtr) -> do
    {# set XIEventMask.deviceid #} maskptr 0
    {# set XIEventMask.mask_len #} maskptr len
    allocaArray (fromIntegral len) $ \mask -> do
      forM list $ addMask mask
      {# set XIEventMask.mask #}     maskptr mask
      callback maskptr

