{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Reflex.Dom.Brownies.PixelCanvas (
    PixelRGBA8(..)
    , ICoord
    , PixelFunction
    , pixelCanvasAttr 
    )
where 

import           Reflex.Dom
import           Reflex.Dom.Brownies.LowLevel (clampedArrayFromPixels)
import           GHCJS.DOM.Types (HTMLCanvasElement(..), CanvasRenderingContext2D(..), toJSVal, liftDOM)
import           GHCJS.DOM.HTMLCanvasElement(getWidth, getHeight, getContext)
import           GHCJS.DOM.ImageData (newImageData)
import           GHCJS.DOM.CanvasRenderingContext2D (putImageData)
import           Language.Javascript.JSaddle(Object, JSM) 


import qualified Data.Map as M (Map)
import qualified Data.Text as T
import           Data.Maybe(fromJust)

import           Data.Word
import           Data.Bits(shiftL)
import qualified Data.Vector as V

-- --------------------------------------------------------------------------
-- Canvas 
-- --------------------------------------------------------------------------
--
-- Inspired by the library https://github.com/MaiaVictor/ReflexScreenWidget
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
    cnvs <- liftDOM $ toJSVal (_element_raw canvasEl)
    let canvasElement = HTMLCanvasElement cnvs 
    width <- fromIntegral <$> getWidth canvasElement
    height <- fromIntegral <$> getHeight canvasElement

    -- use ByteString
    -- let evBS = createPixelBS width height <$> evPixFun

    -- use Pixels Words
    let evBS = createPixels width height <$> evPixFun


    -- IO action that will draw our pixels to the canvas 
    -- Each byte of the buffer represents a color channel from 0~255, in the following format:
    -- [0xRR,0xGG,0xBB,0xAA, 0xRR,0xGG,0xBB,0xAA...]. The length of the ByteString
    -- must, thus, be equal to `width * height * 4`. This unsafely casts the
    -- ByteString to a C Ptr that will be used directly on the JS putImageData function.

    -- Draw the canvas, when an draw event occurs
    -- performEvent_ $ liftDOM . putByteString canvasElement width height <$> evBS

    performEvent_ $ liftDOM . writePixelsToCanvas canvasElement width height <$> evBS

    return canvasEl

type ICoord = (Int, Int)

-- | Create Pixels
createPixels :: Int -> Int -> PixelFunction ->  V.Vector Word32
createPixels w h pxf = 
    V.unfoldrN (w * h) (step w h pxf) (0,0)


step :: Int -> Int -> PixelFunction -> ICoord -> Maybe (Word32, ICoord) 
step w h pxf (r,c) 
    | r == h    = Nothing  
    | c < w -1  = Just (rgba, (r, c  + 1))     
    | otherwise = Just (rgba, (r + 1, 0)) 
 where 
    rgba = buildWord $ pxf w h r c

-- Create a word from a Pixel 
buildWord :: PixelRGBA8 -> Word32 
buildWord (PixelRGBA8 rr gg bb aa) =  fromIntegral rr 
    + (shiftL (fromIntegral gg) 8)
    + (shiftL (fromIntegral bb) 16)
    + (shiftL (fromIntegral aa) 24)

-- | Write the pixels to the canvas (Replacement for putByteString)
writePixelsToCanvas :: HTMLCanvasElement -> Int -> Int -> V.Vector Word32 -> JSM ()
writePixelsToCanvas htmlCanvas width height pixels = do
    ctx <-  getContext htmlCanvas ("2d" :: String) ([] :: [Object])
    jsvalCtx <- toJSVal $ fromJust ctx
    let ctx2d = CanvasRenderingContext2D jsvalCtx
    clampedArray <- clampedArrayFromPixels pixels
    imageData <- newImageData clampedArray (fromIntegral width) (Just $ fromIntegral height)
    _ <- putImageData ctx2d imageData 0 0
    return ()