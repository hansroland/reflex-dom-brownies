{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction, JavaScriptFFI, CPP #-}

module Reflex.Dom.Brownies.LowLevel (
    alertEvent
    , drawImage
    , draw

    ) where

import           Reflex.Dom
import           GHCJS.DOM.Types (unElement, toElement, toJSString, liftDOM, MonadDOM, JSVal)
import           Foreign.Ptr (Ptr)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Unsafe as BS (unsafeUseAsCString)

#ifdef __GHCJS__
import GHCJS.Types (JSString)

import GHCJS.Marshal.Pure (pToJSVal)
-- import Control.Monad.IO.Class (liftIO)
#else
import Language.Javascript.JSaddle(jsg1)
#endif

-- ----------------------------------------------------------------------------------
-- alert
-- ----------------------------------------------------------------------------------
alertEvent :: MonadWidget t m => (a -> String) -> Event t a -> m ()

#ifdef __GHCJS__

-- Taken from the `reflex-dom-contrib` package.
alertEvent eventValueToStr e = performEvent_ (alert <$> e)
  where
    -- alert :: (MonadIO m) => String -> MonadDOM ()
    alert a = liftDOM $ js_alert $ toJSString $ eventValueToStr a

foreign import javascript unsafe
  "alert($1)"
  js_alert :: JSString -> MonadDOM ()

#else

alertEvent eventValueToStr e = performEvent_ (jsAlert <$> e)
    where 
       jsAlert a = do 
         _ <- liftDOM $ jsg1 ("alert" :: String) $ toJSString $ eventValueToStr a
         return  ()
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