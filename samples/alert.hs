{-# LANGUAGE OverloadedStrings #-}

import Reflex.Dom.Brownies
import GHCJS.DOM.EventM (mouseOffsetXY)
import qualified Data.Text as T
import Data.Monoid ((<>))

main :: IO ()
main = mainWidget $ do
  el "h2" $ text "A n alert example"
  evClick <- button "Click"
  el "br" blank
  alertEvent (\_ -> "Hello I'm an alert") evClick
  return ()
