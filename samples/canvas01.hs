{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

import Reflex.Dom.Brownies
import GHCJS.DOM.EventM (mouseOffsetXY)
import qualified Data.Text as T
import Data.Monoid ((<>))

main :: IO ()
main = mainWidget $ do
  el "h2" $ text "A first canvas example"
  evPostBuild <- getPostBuild
  evRed <- button "Red"
  evGreen <- button "Green"
  el "br" blank
  let evImg = leftmost [greenPixels <$ evPostBuild, redPixels <$ evRed, greenPixels <$ evGreen]
  let attr = "width" =: "256" <> "height" =: "256"
  canvas <- pixelCanvasAttr attr evImg
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

-- Function applied to every index pair
redPixels :: ICoord -> ICoord -> PixelRGBA8
redPixels _  px = PixelRGBA8 (fromIntegral $ snd px) 0 0 255

greenPixels :: ICoord -> ICoord -> PixelRGBA8
greenPixels _ px  = PixelRGBA8 0 (fromIntegral $ fst px) 0 255