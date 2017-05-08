{-# LANGUAGE OverloadedStrings #-}

module Reflex.Dom.Brownies.PixelCanvas (
    PixelRGBA8(..)
    , ICoord
    , PixelFunction
    , pixelCanvasAttr 
    )
where 

import           Control.Monad.IO.Class (liftIO)
import           Reflex.Dom.Brownies.LowLevel (putImageData, draw)
import           GHCJS.DOM.Types (unElement, toElement, toJSVal, HTMLCanvasElement(..))
import           GHCJS.DOM.HTMLCanvasElement(getWidth, getHeight)
import           Reflex.Dom
import           Data.Word8
import           ByteString.StrictBuilder
import           Data.Monoid
import qualified Data.ByteString as BS (ByteString)
import qualified Data.Map as M (Map, empty)
import qualified Data.Text as T

-- --------------------------------------------------------------------------
-- Canvas 
-- --------------------------------------------------------------------------
--
-- Inspired by the library https://github.com/MaiaVictor/ReflexScreenWidget
--          (Code to write to canvas originates from MaiaVictor!)
--
-- Main difference: The library of MaiaVictor always recreates the image.
--                  This library recreates the image only if an Event occurs.
--
-- TODO:
--   add event handling to mouse events
--   make canvas an own type (ev newtype)
--

-- | A Pixel representation with red green blue and alpha channel
data PixelRGBA8 = PixelRGBA8 Word8 Word8 Word8 Word8
  deriving (Show, Eq)

-- | A type synonym for pixel coordinates
type ICoord = (Int, Int)

-- | A function that computes Pixels
type PixelFunction = ICoord   -- ^ size of pixel image)
  -> ICoord                   -- ^ coordinates of current pixel
  -> PixelRGBA8

-- | Renders a Canvas using a PixelFunction. 
--   The canvas is refreshed at every event. Returns the canvas.
pixelCanvasAttr :: MonadWidget t m => M.Map T.Text T.Text -> Event t PixelFunction -> m (El t)
pixelCanvasAttr attrs evPixFun = do
    -- Creates the canvas element on which we will render
    (canvasEl, _) <- elAttr' "canvas" attrs (text "")
    -- Gets the proper GHCJS's JSVal of the canvas
    el <- liftIO $ toJSVal (_element_raw canvasEl)
    let canvasElement = HTMLCanvasElement el 
    wWidth <- getWidth canvasElement
    wHeight <- getHeight canvasElement
    let width = fromIntegral wWidth
    let height = fromIntegral wHeight
    let evBS = pixelByteString width height <$> evPixFun
    -- let canvasJS = unElement.toElement._element_raw $ canvasEl
    -- IO action that will draw our pixels to the canvas 
    -- Each byte of the buffer represents a color channel from 0~255, in the following format:
    -- [0xRR,0xGG,0xBB,0xAA, 0xRR,0xGG,0xBB,0xAA...]. The length of the ByteString
    -- must, thus, be equal to `width * height * 4`. This unsafely casts the
    -- ByteString to a C Ptr that will be used directly on the JS putImageData function.

    -- Draw the canvas, when an draw event occurs
    performEvent_ $ liftIO . draw canvasEl width height <$> evBS
    return canvasEl

-- Create an image with a pixel function
pixelByteString :: Int -> Int -> PixelFunction -> BS.ByteString
pixelByteString width height pxf = builderBytes $ foldMap renderPixel $ pixelList width height
  where
    pixelList :: Int -> Int -> [PixelRGBA8]
    pixelList w h = [pxf (w, h) (c, r) | r <- [0..h - 1], c <- [0..w - 1] ]
    renderPixel :: PixelRGBA8 -> Builder
    renderPixel (PixelRGBA8 r g b a) = word8 r <> word8 g <> word8 b <> word8 a