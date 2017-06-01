{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction, JavaScriptFFI, CPP #-}

module Reflex.Dom.Brownies.LowLevel (
    drawImage
    , draw

    ) where

import           Reflex.Dom
import           GHCJS.DOM.Types (unElement, toElement, toJSString, liftDOM, JSVal)
import           Foreign.Ptr (Ptr)
import           Language.Javascript.JSaddle(jsg1)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Unsafe as BS (unsafeUseAsCString)

#ifdef __GHCJS__
import GHCJS.Types (JSString)
import GHCJS.Marshal.Pure (pToJSVal)
#endif

-- ----------------------------------------------------------------------------------
-- putImageData
-- ----------------------------------------------------------------------------------
draw :: El t -> Int -> Int -> BS.ByteString -> IO ()
draw canvasEl width height pixelByteString =
            BS.unsafeUseAsCString pixelByteString $ \ ptr ->
                drawImage canvasJS width height ptr
          where canvasJS = unElement.toElement._element_raw $ canvasEl

#ifdef __GHCJS__

drawImage :: forall a . JSVal -> Int -> Int -> Ptr a -> IO ()
drawImage canvas width height pixels 
  = js_putImageData canvas (pToJSVal width) (pToJSVal height) pixels

-- Code is Copyright by Victor Hernandes Silva Maia
-- Code copied from commit daac065 of of 
--   https://github.com/MaiaVictor/ReflexScreenWidget from file Screen.hs
foreign import javascript unsafe 
    -- Arguments
    --    canvas : JSHtmlElementCanvas
    --    width  : JSNumber
    --    height : JSNumber
    --    pixels : Ptr a -- Pointer to a ByteString in the format below
    "(function(){                                                     \
        var cvs    = $1;                                              \
        var width  = $2;                                              \
        var height = $3;                                              \
        var pixels = new Uint8ClampedArray($4.u8);                    \
        cvs.width  = width;                                           \
        cvs.height = height;                                          \
        var ctx    = cvs.getContext('2d');                            \
        ctx.putImageData(new ImageData(pixels, width, height), 0, 0); \
    })()"
    -- | Draw a Haskell ByteString to a JavaScript Canvas
    js_putImageData :: forall a . JSVal -> JSVal -> JSVal -> Ptr a -> IO ()

#else
drawImage :: forall a . JSVal -> Int -> Int -> Ptr a -> IO ()
drawImage = error "drawImage can only be used with GHCJS"
#endif