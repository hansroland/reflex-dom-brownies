{-# LANGUAGE OverloadedStrings #-}

module Reflex.Dom.Brownies.Alert (alertEvent)

where

import           Reflex.Dom
import           GHCJS.DOM.Types (toJSString, liftDOM)
import           Language.Javascript.JSaddle(jsg1)
-- ----------------------------------------------------------------------------------
-- alert
-- ----------------------------------------------------------------------------------
alertEvent :: MonadWidget t m => (a -> String) -> Event t a -> m ()
alertEvent eventValueToStr e = performEvent_ (jsAlert <$> e)
    where 
       jsAlert a = do 
         _ <- liftDOM $ jsg1 ("alert" :: String) $ toJSString $ eventValueToStr a
         return  ()