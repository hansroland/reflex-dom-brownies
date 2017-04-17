{-# LANGUAGE OverloadedStrings #-}

module Reflex.Dom.Brownies.Widgets

where 

import Control.Monad.IO.Class (liftIO)
import Reflex.Dom.Brownies.LowLevel (blitByteString)
import GHCJS.DOM.Types (unElement, toElement)
import GHCJS.Marshal.Pure (pToJSVal)
import Reflex.Dom
import Data.Word8
import ByteString.StrictBuilder
import Data.Monoid

import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Unsafe as BS (unsafeUseAsCString)
import qualified Data.Map as M (Map, empty)
import qualified Data.Text as T

-- --------------------------------------------------------------------------
-- Canvas 
-- --------------------------------------------------------------------------
--
-- Inspired by the library https://github.com/MaiaVictor/ReflexScreenWidget
--          (still a lot of code originates from MaiaVictor!)
--
-- Main difference: The library of MaiaVictor always recreates the image.
--                  This library recreates the image only if an Event occurs.
--
-- TODO:
--   widht and heigth should be stored in the canvas not the image
--   add event handling fo mouse events
--   make canvas an own type (ev newtype)
--
-- | The image format used to render to the canvas. Each byte of the buffer
-- represents a color channel from 0~255, in the following format:
-- [0xRR,0xGG,0xBB,0xAA, 0xRR,0xGG,0xBB,0xAA...]. The length of the ByteString
-- must, thus, be equal to `width * height * 4`. This unsafely casts the
-- ByteString to a C Ptr that will be used directly on the JS blitting
-- function.
data ByteImageRgba = ByteImageRgba { 
    _width  :: Int,
    _height :: Int,
    _buffer :: BS.ByteString}

-- | A Pixel representation with red green blue and alpha channel
data PixelRGBA = PixelRGBA Word8 Word8 Word8 Word8
  deriving (Show, Eq)

-- | Create an image with a pixel function
createImage :: Int -> Int -> ((Int, Int) -> PixelRGBA) -> ByteImageRgba
createImage width height pxf = ByteImageRgba width height $ imageBS width height pxf
  where
    indexArray :: Int -> Int -> [(Int, Int)]
    indexArray w h = [(r, c) | r <- [h -1, h - 2..0], c <- [w -1, w -2..0] ]
    renderPixel :: PixelRGBA -> Builder
    renderPixel (PixelRGBA r g b a) = word8 r <> word8 g <> word8 b <> word8 a
    imageBS :: Int -> Int -> ((Int, Int) -> PixelRGBA) -> BS.ByteString
    imageBS w h f = builderBytes $ foldMap (renderPixel . f) $ indexArray w h

-- | Renders a dynamic ByteImageData using a Canvas. The canvas is refreshed
--   at every event. Returns the canvas.
screenWidgetAttr :: MonadWidget t m => M.Map T.Text T.Text -> Behavior t ByteImageRgba -> Event t a -> m (El t)
screenWidgetAttr attrs imageBehavior event = do

    -- Creates the canvas element on which we will render
    (canvasEl, _) <- elAttr' "canvas" attrs (text "")

    -- Gets the proper GHCJS's JSVal of the canvas
    let canvasJS = unElement.toElement._element_raw $ canvasEl

    -- IO action that will draw our pixels to the canvas 
    let draw :: ByteImageRgba -> IO ()
        draw (ByteImageRgba width height pixelByteString) =
            BS.unsafeUseAsCString pixelByteString $ \ ptr ->
                blitByteString canvasJS (pToJSVal width) (pToJSVal height) ptr

    -- Draw the canvas, when an draw event occurs
    performEvent_ $ liftIO . draw <$> tag imageBehavior event
    return canvasEl

-- | Same as above, without the Attr argument.
screenWidget :: MonadWidget t m => Behavior t ByteImageRgba -> Event t a -> m (El t)
screenWidget = screenWidgetAttr M.empty