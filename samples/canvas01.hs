{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

import Reflex.Dom.Brownies
import GHCJS.DOM.EventM (mouseOffsetXY)
import qualified Data.Text as T 

main :: IO ()
main = mainWidget $ do
  el "h2" $ text "A first canvas example"
  evPostBuild <- getPostBuild
  evRed <- button "Red"
  evGreen <- button "Green"
  el "br" blank
  let evImg = leftmost [greenImage <$ evPostBuild, redImage <$ evRed, greenImage <$ evGreen]
  canvas <- pixelCanvas evImg
  return ()
  
  {-
  mouseEvent <- wrapDomEvent 
    (_element_raw canvas) 
    (onEventName Mousemove) 
    mouseOffsetXY

  let mouseXYToString (x,y) = "X = " ++ show x ++ ";Y = " ++ show y
  t <- holdDyn "" (T.pack.mouseXYToString <$> mouseEvent)
  el "div" $ dynText t
  -}

  return ()

-- Pure user user functions ------------------------------------------------------------------------------

redImage :: ByteImageRgba
redImage = createImage 512 256 redPixels

greenImage :: ByteImageRgba
greenImage = createImage 512 256 greenPixels

-- Function applied to every index pair
redPixels :: (Int, Int) -> PixelRGBA
redPixels (x,_) = PixelRGBA (fromIntegral x) 0 0 255

greenPixels :: (Int, Int) -> PixelRGBA
greenPixels (x,_) = PixelRGBA 0 (fromIntegral x) 0 255