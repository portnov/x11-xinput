
import Control.Monad
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
