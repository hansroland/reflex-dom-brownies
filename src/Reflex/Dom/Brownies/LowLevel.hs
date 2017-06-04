{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction, JavaScriptFFI, CPP #-}

module Reflex.Dom.Brownies.LowLevel (
    clampedArrayFromBS
    ) where

import           GHCJS.DOM.Types (Uint8ClampedArray(..))
import qualified Data.ByteString as BS (ByteString)

#ifdef __GHCJS__
import           GHCJS.DOM.Types (JSM, pFromJSVal, JSVal)
import           Foreign.Ptr (Ptr)
import qualified Data.ByteString.Unsafe as BS (unsafeUseAsCString)
#else
import           GHCJS.DOM.Types (JSM, liftDOM)
import           GHCJS.Buffer (fromByteString, getArrayBuffer, thaw)
import           Language.Javascript.JSaddle (new, jsg, pToJSVal, ghcjsPure)
#endif

-- | Create a clampedArray from a ByteString
clampedArrayFromBS :: BS.ByteString -> JSM Uint8ClampedArray
#ifdef __GHCJS__
clampedArrayFromBS bs =
    BS.unsafeUseAsCString bs $ \ ptr ->
        newUint8ClampedArray ptr

newUint8ClampedArray :: Ptr a -> JSM Uint8ClampedArray
newUint8ClampedArray ptr = pFromJSVal <$> jsUint8ClampedArray ptr

foreign import javascript unsafe 
    -- Arguments
    --     pixels : Ptr a -- Pointer to a ByteString 
    "(function(){ return new Uint8ClampedArray($1.u8); })()" 
    jsUint8ClampedArray :: Ptr a -> JSM JSVal

#else

clampedArrayFromBS bs = do
  (buffer,_,_) <- ghcjsPure $ fromByteString bs -- fromString converts to 64 encoding
  buffer' <- thaw buffer
  arrbuff <- ghcjsPure (getArrayBuffer buffer')
  liftDOM (Uint8ClampedArray <$> new (jsg "Uint8ClampedArray") [pToJSVal arrbuff])
#endif
