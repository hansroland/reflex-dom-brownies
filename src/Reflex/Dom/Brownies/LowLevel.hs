{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction, JavaScriptFFI, CPP #-}

module Reflex.Dom.Brownies.LowLevel (
    alertEvent
    , putImageData
    , draw

    ) where

import           Reflex.Dom
import           GHCJS.DOM.Types (unElement, toElement, HTMLCanvasElement, castToHTMLCanvasElement)

import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Unsafe as BS (unsafeUseAsCString)

#ifdef __GHCJS__
import GHCJS.DOM.Types (toJSString)
import GHCJS.Types (JSVal, JSString)
import Foreign.Ptr (Ptr)
import GHCJS.Marshal.Pure (pFromJSVal, pToJSVal)
import Control.Monad.IO.Class (liftIO)
#endif

-- ----------------------------------------------------------------------------------
-- alert
-- ----------------------------------------------------------------------------------
alertEvent :: MonadWidget t m => (a -> String) -> Event t a -> m ()

#ifdef __GHCJS__

-- Taken from the `reflex-dom-contrib` package.
alertEvent eventValueToStr e = performEvent_ (alert <$> e)
  where
    -- alert :: (MonadIO m) => String -> IO ()
    alert a = liftIO $ js_alert $ toJSString $ eventValueToStr a

foreign import javascript unsafe
  "alert($1)"
  js_alert :: JSString -> IO ()

#else
alertEvent = error "js_alert can only be used with GHCJS"
#endif

-- ----------------------------------------------------------------------------------
-- putImageData
-- ----------------------------------------------------------------------------------
draw :: El t -> Int -> Int -> BS.ByteString -> IO ()
draw canvasEl width height pixelByteString =
            BS.unsafeUseAsCString pixelByteString $ \ ptr ->
                putImageData canvasJS width height ptr
          where canvasJS = unElement.toElement._element_raw $ canvasEl

#ifdef __GHCJS__

putImageData :: forall a . JSVal -> Int -> Int -> Ptr a -> IO ()
putImageData canvas width height pixels 
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
putImageData = error "putImageData can only be used with GHCJS"
#endif