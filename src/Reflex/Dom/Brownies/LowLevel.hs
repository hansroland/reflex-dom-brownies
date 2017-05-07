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
import Control.Monad.IO.Class (liftIO)


#ifdef __GHCJS__
import GHCJS.DOM.Types (toJSString)
import GHCJS.Types (JSVal, JSString)
import Foreign.Ptr (Ptr)
import GHCJS.Marshal.Pure (pFromJSVal, pToJSVal)
#else


import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase(JSContextRef, jsevaluatescript)
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSStringRef(jsstringcreatewithutf8cstring)
import Graphics.UI.Gtk.WebKit.JavaScriptCore.WebFrame(webFrameGetGlobalContext)
-- import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSObjectRef
import Graphics.UI.Gtk.WebKit.WebView(WebView, webViewGetMainFrame)
import Foreign.Ptr(nullPtr)
import Foreign.JavaScript.TH(unWebViewSingleton, askWebView)
import Reflex.Dom.Class



import Graphics.UI.Gtk.WebKit.DOM.HTMLCanvasElement(toHTMLCanvasElement)

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
alertEvent eventValueToStr e = do
    wv <- fmap unWebViewSingleton askWebView             --  alert :: (MonadIO m) => String -> IO ()
    let alert a = liftIO $ jsAlert wv $ eventValueToStr a
    performEvent_ (alert <$> e)

jsAlert :: WebView -> String -> IO ()
jsAlert vw _ = withWebViewContext vw $ \c -> do
   script <- jsstringcreatewithutf8cstring "alert"
   -- args <- toJSObject c ["Hello I'm a static alert"]
   jsevaluatescript c script nullPtr nullPtr 1 nullPtr  -- Parameter fehlt noch (nullPtr nach script)
   return ()

-- | Copied from Reflex.Dom.Internal.Foreign.hs
withWebViewContext :: WebView -> (JSContextRef -> IO a) -> IO a
withWebViewContext wv f = f =<< webFrameGetGlobalContext =<< webViewGetMainFrame wv
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

-- use fromIntegral to convert Ints
-- use toHTMLCanvasElement :: HTMLCanvasElementClass o => o -> HTMLCanvasElement
#endif