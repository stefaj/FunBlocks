{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}

module CodeGen
  where

import Blockly.Block
import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Foreign.Callback
import Data.JSString (pack, unpack)
import Data.Maybe (fromJust)
import GHCJS.Marshal
import Unsafe.Coerce

setCodeGen :: String -> (Block -> String) -> IO ()
setCodeGen blockName func = do
  cb <- syncCallback1' (\x -> do Just b <- fromJSVal x 
                                 toJSVal $ pack $ func b)
  js_setGenFunction (pack blockName) cb


--- FFI
foreign import javascript unsafe "Blockly.FunBlocks[$1] = $2"
  js_setGenFunction :: JSString -> Callback a -> IO ()
