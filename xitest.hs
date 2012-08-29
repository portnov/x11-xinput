
import Control.Monad
import Control.Concurrent (threadDelay)
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
    devices <- queryDevice dpy XIAllDevices 
    forM_ devices print
    let dflt = defaultScreen dpy
        border = blackPixel dpy dflt
        background = whitePixel dpy dflt
    rootw <- rootWindow dpy dflt
    win <- createSimpleWindow dpy rootw 0 0 100 100 1 border background
    selectInput dpy win exposureMask
    setEventMask dpy win [XI_Enter, XI_Leave, XI_ButtonPress, XI_ButtonRelease]
    setTextProperty dpy win "Hello World" wM_NAME
    mapWindow dpy win
    sync dpy False
    allocaXEvent $ \eptr ->
      forever $ do
        nextEvent dpy eptr
        handleXCookie dpy xi_opcode eptr evHandler cookieHandler

evHandler e = print "Event"

cookieHandler e = print e
