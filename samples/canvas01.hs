{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

import Reflex.Dom.Brownies
-- import GHCJS.DOM.EventM (mouseOffsetXY)
import qualified Data.Text as T ()
import Data.Monoid ((<>))

main :: IO ()
main = mainWidget $ do
  el "h2" $ text "A first canvas example"
  evPostBuild <- getPostBuild
  evRed <- button "Red"
  evGreen <- button "Green"
  el "br" blank
  let evImg = leftmost [greenPixels <$ evPostBuild, redPixels <$ evRed, greenPixels <$ evGreen]
  -- let attr = "width" =: "1024" <> "height" =: "1024"
  let attr = "width" =: "256" <> "height" =: "256"
  _ <- pixelCanvasAttr attr evImg
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
redPixels :: Int -> Int -> Int -> Int -> PixelRGBA8
redPixels _ _ _ py = PixelRGBA8 (fromIntegral py) 0 0 255

greenPixels :: Int -> Int -> Int -> Int -> PixelRGBA8
greenPixels _ _ px _  = PixelRGBA8 0 (fromIntegral px) 0 255