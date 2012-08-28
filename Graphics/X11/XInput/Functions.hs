
module Graphics.X11.XInput.Functions where

import Control.Applicative
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import qualified Graphics.X11 as X11
import Graphics.X11.Xlib.Extras

import Graphics.X11.XInput.Types

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

