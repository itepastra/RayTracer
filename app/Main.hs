{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import SDL.Vect
import SDL.Video
import SDL.Raw.Video (videoInit)
import Foreign.C (newCAString)

main :: IO ()
main = do

    i <- videoInit =<< newCAString ""

    w <- mkwindow
    putStrLn "made a window"

    print w
    r <- createRenderer w 0 defaultRenderer

    print r
    fillRect r (Just (Rectangle (P (V2 30 30)) (V2 200 200)))

    present r
    print r

    loop 100000000000000
    destroyWindow w
    putStrLn "destroyed the window"

loop :: (Ord t, Num t, Applicative f) => t -> f ()
loop a | a > 0 = loop (a -1)
       | otherwise = pure ()

-- initii = createRGBSurface (V2 1280 720) RGBA8888

mkwindow :: IO Window
mkwindow = createWindow "testWindow" windowConfig

windowConfig :: WindowConfig
windowConfig =
    WindowConfig
        { windowBorder = True
        , windowHighDPI = False
        , windowInputGrabbed = False
        , windowMode = Windowed
        , windowGraphicsContext = OpenGLContext (OpenGLConfig (V4 8 8 8 0) 24 8 1 (Compatibility Normal 3 1))
        -- , windowGraphicsContext = NoGraphicsContext
        -- , windowGraphicsContext = VulkanContext
        , windowPosition = Wherever
        , windowResizable = False
        , windowInitialSize = V2 1600 900
        , windowVisible = True
        }