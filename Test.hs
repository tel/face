{-# LANGUAGE OverloadedStrings, JavaScriptFFI #-}

import Web.Face

import Control.Event.Handler
import Data.String
import Data.Time
import GHCJS.Foreign
import GHCJS.Prim
import GHCJS.Types
import Reactive.Banana
import Reactive.Banana.Frameworks
import Web.Face

data El_ = El_

foreign import javascript safe
  "$1.insertBefore($2, $3)"
  _insertBefore :: JSRef El_ -> JSRef El_ -> JSRef El_ -> IO (JSRef El_)

foreign import javascript unsafe
  "document.getElementById($1)"
  _getById :: JSString -> IO (JSRef El_)

foreign import javascript safe
  "$1.innerHTML = $2"
  _setInnerHtml :: JSRef El_ -> JSString -> IO ()

foreign import javascript unsafe
  "var f = function () { $1(); window.requestAnimationFrame(f); }; window.requestAnimationFrame(f);"
  _requestAnimationFrame :: JSFun (IO ()) -> IO ()

foreign import javascript unsafe
  "$1.onclick = $2"
  _setOnClick :: JSRef El_ -> JSFun (IO ()) -> IO ()

raf :: ForeignRetention -> Bool -> IO () -> IO ()
raf retain block io = syncCallback retain block io >>= _requestAnimationFrame

printIt :: Show a => a -> IO ()
printIt t = do
  el <- _getById "target"
  _setInnerHtml el (fromString (show t))

makeCounter :: IO () -> IO ()
makeCounter act = do
  el <- _getById "touch"
  _setOnClick el =<< syncCallback NeverRetain False act



main = do
  (renderEvent, render) <- newAddHandler
  (countEvent, count) <- newAddHandler
  z0 <- getCurrentTime
  let
    network :: Frameworks t => Moment t ()
    network = do
      eRender <- fromAddHandler renderEvent
      eCount  <- fromAddHandler countEvent
      let eTime = stepper z0 eRender
      reactimate (printIt <$> (eTime <@ eCount))
  makeCounter (count ())
  actuate =<< compile network
  count ()
  raf NeverRetain False (render =<< getCurrentTime)
  putStrLn "hello world---reactively!"
