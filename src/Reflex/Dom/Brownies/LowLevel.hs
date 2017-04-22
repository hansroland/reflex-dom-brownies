{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction, JavaScriptFFI, CPP #-}

module Reflex.Dom.Brownies.LowLevel (
    blitByteString
    , js_alert 
    ) where
--

import Foreign.Ptr (Ptr)
import GHCJS.Types (JSVal, JSString)
import GHCJS.DOM.Types (toJSString)


#ifdef __GHCJS__

foreign import javascript unsafe
  "alert($1)"
  js_alert :: JSString -> IO ()

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
    blitByteString :: forall a . JSVal -> JSVal -> JSVal -> Ptr a -> IO ()

#endif