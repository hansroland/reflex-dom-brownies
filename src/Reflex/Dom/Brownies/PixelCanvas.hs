{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Reflex.Dom.Brownies.PixelCanvas (
    PixelRGBA8(..)
    , ICoord
    , PixelFunction
    , pixelCanvasAttr 
    )
where 

import           Control.Monad.IO.Class (liftIO)
import           Reflex.Dom.Brownies.LowLevel (draw)
import           GHCJS.DOM.Types (toJSVal, HTMLCanvasElement(..), MonadDOM, liftDOM)
import           GHCJS.DOM.HTMLCanvasElement(getWidth, getHeight)
import           Reflex.Dom
import           Data.Word8
import qualified Data.ByteString as BS (ByteString, empty)
import qualified Data.ByteString.Internal as BSI
import qualified Data.Map as M (Map)
import qualified Data.Text as T

import           Foreign.Ptr (plusPtr)
import           System.IO.Unsafe (unsafePerformIO)
import           Foreign.Storable (poke, pokeByteOff)

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

-- | Generate RGBA-ByteString for our image
pixelByteString :: Int -> Int -> PixelFunction -> BS.ByteString
pixelByteString width height pxf = fst $ unfoldrX (width * height) (step pxf width height) (0, 0)

type ICoord = (Int, Int)

-- | Low level function to create a ByteString.
--   Similar to BS.unfoldrN but specialiced to PixelRGBA: 
--   For every Pixel we store 4 bytes to the ByteString
unfoldrX :: Int ->                           -- ^ Number of Pixels
         (a -> (PixelRGBA8, a))              -- ^ Stepping function
         -> a                                -- ^ Start argument for stepping function
         -> (BS.ByteString, a)
unfoldrX ip pxf x0
    | nb < 0    = (BS.empty, x0)             -- nb = number of bytes; np = number of pixels
    | otherwise = unsafePerformIO $ BSI.createAndTrim' nb $ \p -> go p x0 0
  where 
    nb = 4 * ip
    go !p !x !n
        | n == nb    = return (0, n, x)
        | otherwise = do poke p rr
                         pokeByteOff p 1 gg
                         pokeByteOff p 2 bb
                         pokeByteOff p 3 aa
                         go (p `plusPtr` 4) x' (n+4)
      where (PixelRGBA8 rr gg bb aa, x') = pxf x

-- | Stepping function for unfoldrX
-- It steps through the index space of the image and calls for every index the pixel function
step :: PixelFunction -> Int -> Int -> ICoord -> (PixelRGBA8, ICoord)
step pxf w h (r, c)
    | r < h -1  = (rgba, (r + 1, c))                 
    | otherwise = (rgba, (0, c + 1))               

--    | c < w -1  = (rgba, (r, c + 1))                -- step through one row
--    | otherwise = (rgba, (r + 1, 0))                -- step through all rows
  where 
    rgba = pxf w h r c