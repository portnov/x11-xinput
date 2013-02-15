{-# LANGUAGE RecordWildCards #-}
{-| This module contains some higher-level functions,
 - wrapping XInput calls. #-}
module Graphics.X11.XInput.Functions
  (xinputInit,
   xinputCheckVersion,
   handleXCookie,
   eventButton,
   eventWindow,
   eventKeyMask,
   eventMousePos
  ) where

import Control.Applicative
import Control.Monad.Trans
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import qualified Graphics.X11 as X11
import qualified Graphics.X11.Xlib.Extras as E

import Graphics.X11.XInput.Types
import Graphics.X11.XInput.Foreign
import Graphics.X11.XInput.Parser

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

-- | Handle usual X11 event or cookie event.
handleXCookie :: (MonadIO m) => X11.Display
                             -> Opcode               -- ^ Extension identifier (one got from xinputInit)
                             -> X11.XEventPtr        -- ^ Pointer to X11 event
                             -> (E.Event -> m a)     -- ^ Handler for usual X11 event
                             -> (EventCookie -> m a) -- ^ Handler for X11 cookie event
                             -> m a
handleXCookie dpy xi_opcode xev evHandler cookieHandler = do
--   liftIO $ putStrLn "handling XCookie"
  evtype <- liftIO $ get_event_type xev
  ext    <- liftIO $ get_event_extension xev
  hasCookie <- liftIO $ getEventData dpy (castPtr xev)
  result <- if (evtype == genericEvent) && (ext == xi_opcode) && hasCookie
              then do
--                    liftIO $ putStrLn "XInput event"
                   cookieHandler =<< (liftIO $ getXGenericEventCookie xev)
              else evHandler =<< (liftIO $ E.getEvent xev)
  liftIO $ freeEventData dpy (castPtr xev)
  return result

-- | Shortcut to get button number or keycode
-- from keyboard or mouse event. Returns Nothing
-- if this is not mouse or keyboard event.
eventButton :: Event -> Maybe Int
eventButton (Event {..})
  | (eType `elem` [XI_ButtonPress, XI_ButtonRelease,
                   XI_KeyPress, XI_KeyRelease]) =
        case eSpecific of
          GPointerEvent {peDetail = n} -> Just n
          _                            -> Nothing
  | otherwise = Nothing

-- | Shortcut to get pointer position from event
eventMousePos :: Event -> Maybe (X11.Position, X11.Position)
eventMousePos (Event {..})
  | (eType `elem` [XI_ButtonPress, XI_ButtonRelease,
                   XI_KeyPress, XI_KeyRelease,
                   XI_Enter, XI_Leave]) =
        let x = round (peRootX eSpecific)
            y = round (peRootY eSpecific)
        in  Just (x, y)
  | otherwise = Nothing

-- | Shortcut to get event window.
-- Returns Nothing if this is not pointer-related event
eventWindow :: Event -> Maybe X11.Window
eventWindow e =
  case eSpecific e of
    GPointerEvent {peEvent = w} -> Just w
    _                           -> Nothing

-- | Shortcut to get keymask from event
eventKeyMask :: Event -> Maybe X11.KeyMask
eventKeyMask (Event {eSpecific = GPointerEvent {peSpecific = e}}) =
  Just $ fromIntegral $ msBase $ peMods e
eventKeyMask _ = Nothing

