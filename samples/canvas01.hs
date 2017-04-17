{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

import Reflex.Dom.Brownies

main :: IO ()
main = mainWidget $ do
  redScreen
  return ()

redScreen :: MonadWidget t m => m ()
redScreen = do
  let image = createImage 512 256 myColorFunction
  _ <- screenWidget (constant image) =<< getPostBuild
  return ()

-- Pure user user functions ------------------------------------------------------------------------------

-- Function applied to every index pair
myColorFunction :: (Int, Int) -> PixelRGBA
myColorFunction (x,_) = PixelRGBA (fromIntegral x) 0 0 255