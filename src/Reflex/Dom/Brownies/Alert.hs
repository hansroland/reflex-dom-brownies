{-# LANGUAGE OverloadedStrings #-}

module Reflex.Dom.Brownies.Alert

where

import Reflex.Dom
import Control.Monad.IO.Class (liftIO)
import GHCJS.DOM.Types (toJSString)
import Reflex.Dom.Brownies.LowLevel(js_alert)

-- | Convenient function that pops up a javascript alert dialog box when an
-- event fires with a message to display.
--
-- Taken from the `reflex-dom-contrib` package.
alertEvent :: MonadWidget t m => (a -> String) -> Event t a -> m ()
alertEvent eventValueToStr e = performEvent_ (alert <$> e)
  where
    -- alert :: (MonadIO m) => String -> IO ()
    alert a = liftIO $ js_alert $ toJSString $ eventValueToStr a