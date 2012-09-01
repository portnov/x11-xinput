
import Control.Applicative
import Control.Monad
import Control.Concurrent (threadDelay)
import Data.Bits
import Graphics.X11
import Graphics.X11.XInput

withDisplay :: String -> (Display -> IO a) -> IO ()
withDisplay str action = do
  dpy <- openDisplay str
  action dpy
  closeDisplay dpy

main = do
  withDisplay ":0" $ \dpy -> do
    InitOK xi_opcode <- xinputInit dpy
    devices <- buildDevicesMap <$> queryDevice dpy XIAllDevices 
    putStrLn (showDevicesMap devices)
    let dflt = defaultScreen dpy
        border = blackPixel dpy dflt
        background = whitePixel dpy dflt
    rootw <- rootWindow dpy dflt
    win <- createSimpleWindow dpy rootw 0 0 100 100 1 border background
    selectInput dpy win (exposureMask .|. buttonPressMask .|. buttonReleaseMask)
    setEventMask dpy win [XI_Enter, XI_Leave,
                          XI_ButtonPress, XI_ButtonRelease,
                          XI_KeyPress, XI_KeyRelease]
    setTextProperty dpy win "Hello World" wM_NAME
    mapWindow dpy win
    sync dpy False
    allocaXEvent $ \eptr ->
      forever $ do
        nextEvent dpy eptr
        handleXCookie dpy xi_opcode eptr evHandler cookieHandler
        sync dpy False

evHandler e =
  putStrLn $ "X11 event: " ++ show e

cookieHandler e = print e
