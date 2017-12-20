{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction, JavaScriptFFI, CPP #-}

-- For documentation about the used functions see_
--   https://hackage.haskell.org/package/jsaddle-dom-0.8.0.0/docs/

module Reflex.Dom.Brownies.LowLevel (
    clampedArrayFromBS ,
    clampedArrayFromPixels
    ) where

import           GHCJS.DOM.Types (Uint8ClampedArray(..))
import qualified Data.ByteString as BS (ByteString)
import qualified Data.Vector as V

import           Data.Word 


#ifdef __GHCJS__
import           GHCJS.DOM.Types (JSM, pFromJSVal, JSVal)
import           Foreign.Ptr (Ptr)
import qualified Data.ByteString.Unsafe as BS (unsafeUseAsCString)
#else
import           GHCJS.DOM.Types (JSM, liftDOM)
import           GHCJS.Buffer (getArrayBuffer, MutableBuffer)
import           GHCJS.Buffer.Types(SomeBuffer(..))
import           Language.Javascript.JSaddle (new, jsg, jsg1, pToJSVal, ghcjsPure)
import qualified Data.ByteString.Base64 as B64 (encode)
import           Data.Text.Encoding (decodeUtf8)

-- import qualified Data.ByteString as BS (length)
import           GHCJS.DOM.Types(ArrayBuffer(..))
import           GHCJS.DOM.Types(unsafeCastTo)
import           Language.Javascript.JSaddle ((!), (<##))
import           Control.Lens(itraverse_)


#endif

-- | Create a clampedArray from a ByteString
clampedArrayFromBS :: BS.ByteString -> JSM Uint8ClampedArray
clampedArrayFromPixels :: V.Vector Word32 -> JSM Uint8ClampedArray
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

clampedArrayFromPixels = undefined

#else

clampedArrayFromBS bs = do
  buffer :: MutableBuffer <- SomeBuffer <$> jsg1 "h$newByteArrayFromBase64String"
                               (decodeUtf8 $ B64.encode bs)
  arrbuff <- ghcjsPure $ getArrayBuffer buffer
  liftDOM (Uint8ClampedArray <$> new (jsg "Uint8ClampedArray") [pToJSVal arrbuff])


clampedArrayFromPixels pxs = do 
  arrbuff <- buildBuffer pxs 
  liftDOM (Uint8ClampedArray <$> new (jsg "Uint8ClampedArray") [pToJSVal arrbuff])

-- See: https://qfpl.io/posts/working-with-jsaddle/

buildBuffer :: V.Vector Word32 -> JSM ArrayBuffer
buildBuffer pxs = do
  let
    buffSize :: Double
    buffSize = fromIntegral (length pxs * 4)
  buff <- new (jsg "ArrayBuffer") buffSize
  w32Arr <- new (jsg "Uint32Array") buff
  itraverse_ (w32Arr <##) pxs
  buffVal <- w32Arr ! "buffer"
  unsafeCastTo ArrayBuffer buffVal

#endif
