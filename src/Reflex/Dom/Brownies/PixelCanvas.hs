{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Reflex.Dom.Brownies.PixelCanvas (
    PixelRGBA8(..)
    , PixelFunction
    , pixelCanvasAttr 
    )
where 

import           Control.Monad.IO.Class (liftIO)
import           Reflex.Dom.Brownies.LowLevel (draw)
import           GHCJS.DOM.Types (toJSVal, HTMLCanvasElement(..))
import           GHCJS.DOM.HTMLCanvasElement(getWidth, getHeight)
import           Reflex.Dom
import           Data.Word8
import qualified Data.ByteString as BS (ByteString, unfoldrN)
import qualified Data.Map as M (Map)
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
data PixelRGBA8 = PixelRGBA8 !Word8 !Word8 !Word8 !Word8
  deriving (Show, Eq)

compRed :: PixelRGBA8 -> Word8
compRed (PixelRGBA8 r _ _ _ ) = r

compGreen :: PixelRGBA8 -> Word8
compGreen (PixelRGBA8 _ g _ _ ) = g

compBlue :: PixelRGBA8 -> Word8
compBlue (PixelRGBA8 _ _ b _ ) = b

compAlpha :: PixelRGBA8 -> Word8
compAlpha (PixelRGBA8 _ _ _ a ) = a

-- | A function that computes Pixels
type PixelFunction = Int -> Int   -- ^ size of pixel image)
  -> Int -> Int                   -- ^ coordinates of current pixel
  -> PixelRGBA8

-- | Renders a Canvas using a PixelFunction. 
--   The canvas is refreshed at every event. Returns the canvas.
pixelCanvasAttr :: MonadWidget t m => M.Map T.Text T.Text -> Event t PixelFunction -> m (El t)
pixelCanvasAttr attrs evPixFun = do
    -- Creates the canvas element on which we will render
    (canvasEl, _) <- elAttr' "canvas" attrs (text "")
    -- Gets the proper GHCJS's JSVal of the canvas
    cnvs <- liftIO $ toJSVal (_element_raw canvasEl)
    let canvasElement = HTMLCanvasElement cnvs 
    wWidth <- getWidth canvasElement
    wHeight <- getHeight canvasElement
    let width = fromIntegral wWidth
    let height = fromIntegral wHeight
    let evBS = pixelByteString width height <$> evPixFun
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
pixelByteString width height pxf = fst $ BS.unfoldrN (width * height * 4) (step pxf width height) start
  where 
    start = (0, 0, pxf width height 0 0, 0)

type Loc = (Int, Int, PixelRGBA8, Int)

-- | Helper function for unfoldrN
step :: PixelFunction -> Int -> Int -> Loc -> Maybe (Word8, Loc)
step pxf w h (r, c, rgb, i)
    | i == 1    = Just (compGreen rgb,  (r, c, rgb, i + 1))
    | i == 2    = Just (compBlue  rgb,  (r, c, rgb, i + 1))
    | i == 3    = Just (compAlpha rgb,  (r, c, rgb, i + 1))
    | i == 0    = Just (compRed   rgb,  (r, c, rgb, i + 1))
    | c < w -1  = Just (compRed   rgbc, (r, nextc, rgbc, 1))
    | otherwise = Just (compRed   rgbr, (nextr, 0, rgbr, 1))
  where 
    nextc = c + 1
    nextr = r + 1
    rgbc = pxf w h r nextc
    rgbr = pxf w h nextr 0