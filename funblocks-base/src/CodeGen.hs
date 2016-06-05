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
import qualified JavaScript.Array as JA
import Unsafe.Coerce

setCodeGen :: String -> (Block -> (String, Int) ) -> IO ()
setCodeGen blockName func = do
  cb <- syncCallback1' (\x -> do Just b <- fromJSVal x 
                                 let (code,order) = func b
                                 -- alert(code)
                                 return $ js_makeArray (pack code) order
                                 -- toJSVal $ JA.fromList [v,order]
                                 )
  js_setGenFunction (pack blockName) cb


member code = (code, cORDER_MEMBER)
none code = (code, cORDER_NONE)
-- Blocks
blockText block = member $ "text(\"" ++ arg ++ "\")"
  where
    arg = getFieldValue block "TEXT" 

-- TODO check if it is a number
blockNumber block = member arg 
  where
    arg = getFieldValue block "NUMBER"

blockDrawingOf block = member $ "main = drawingOf(" ++ code ++ ");"
  where
    code = valueToCode block "VALUE" cORDER_ATOMIC

blockCombine block = none $ "(" ++ v1 ++ ") & (" ++ v2 ++ ")"
  where
    v1 = valueToCode block "Comb1" cORDER_ATOMIC
    v2 = valueToCode block "Comb2" cORDER_ATOMIC

blockTranslate block = none $ "translated (" ++ pic ++ "," ++ x ++ "," ++ y ++ ")"
  where
    pic = valueToCode block "PICTURE" cORDER_ATOMIC
    x = valueToCode block "X" cORDER_ATOMIC
    y = valueToCode block "Y" cORDER_ATOMIC

blockCodeMap = [ ("cw_text",blockText)
                ,("cw_translate", blockTranslate)
                ,("cw_combine", blockCombine)
                ,("cw_drawingOf", blockDrawingOf)
                ,("cw_number",blockNumber)
                ]

assignAll = mapM (\(n,f) -> setCodeGen n f) blockCodeMap


valueToCode :: Block -> String -> Int -> String
valueToCode block name order = unpack $ js_valueToCode block (pack name) order



--- FFI
foreign import javascript unsafe "Blockly.FunBlocks[$1] = $2"
  js_setGenFunction :: JSString -> Callback a -> IO ()


foreign import javascript unsafe "Blockly.FunBlocks.valueToCode($1, $2, $3)"
  js_valueToCode :: Block -> JSString -> Int -> JSString

-- Ugly hack
foreign import javascript unsafe "[$1,$2]"
  js_makeArray :: JSString -> Int -> JSVal



alert text = js_alert $ pack text
foreign import javascript unsafe "alert($1)" js_alert :: JSString -> IO ()





-- Constants
cORDER_ATOMIC = 0;         -- 0 "" ...
cORDER_MEMBER = 1;         -- . []
cORDER_NEW = 1;            -- new
cORDER_FUNCTION_CALL = 2;  -- ()
cORDER_INCREMENT = 3;      -- ++
cORDER_DECREMENT = 3;      -- --
cORDER_LOGICAL_NOT = 4;    -- !
cORDER_BITWISE_NOT = 4;    -- ~
cORDER_UNARY_PLUS = 4;     -- +
cORDER_UNARY_NEGATION = 4; -- -
cORDER_TYPEOF = 4;         -- typeof
cORDER_VOID = 4;           -- void
cORDER_DELETE = 4;         -- delete
cORDER_MULTIPLICATION = 5; -- *
cORDER_DIVISION = 5;       -- /
cORDER_MODULUS = 5;        -- %
cORDER_ADDITION = 6;       -- +
cORDER_SUBTRACTION = 6;    -- -
cORDER_BITWISE_SHIFT = 7;  -- << >> >>>
cORDER_RELATIONAL = 8;     -- < <= > >=
cORDER_IN = 8;             -- in
cORDER_INSTANCEOF = 8;     -- instanceof
cORDER_EQUALITY = 9;       -- == != === !==
cORDER_BITWISE_AND = 10;   -- &
cORDER_BITWISE_XOR = 11;   -- ^
cORDER_BITWISE_OR = 12;    -- |
cORDER_LOGICAL_AND = 13;   -- &&
cORDER_LOGICAL_OR = 14;    -- ||
cORDER_CONDITIONAL = 15;   -- ?:
cORDER_ASSIGNMENT = 16;    -- = += -= *= /= %= <<= >>= ...
cORDER_COMMA = 17;         -- ,
cORDER_NONE = 99;          -- (...)
















