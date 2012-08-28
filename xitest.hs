
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
    xinput <- xinputInit dpy
    print xinput
    devices <- queryDevice dpy XIAllDevices 
    forM_ devices print
    let dflt = defaultScreen dpy
        border = blackPixel dpy dflt
        background = whitePixel dpy dflt
    rootw <- rootWindow dpy dflt
    win <- createSimpleWindow dpy rootw 0 0 100 100 1 border background
    setTextProperty dpy win "Hello World" wM_NAME
    mapWindow dpy win
    sync dpy False
    threadDelay (10 * 1000000)
