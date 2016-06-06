{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}

module CodeGen (assignAll)
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

setCodeGen :: String -> (Block -> (String, OrderConstants) ) -> IO ()
setCodeGen blockName func = do
  cb <- syncCallback1' (\x -> do Just b <- fromJSVal x 
                                 let (code,ordr) = func b
                                 -- alert(code)
                                 return $ js_makeArray (pack code) (order ordr)
                                 -- toJSVal $ JA.fromList [v,order]
                                 )
  js_setGenFunction (pack blockName) cb


-- Helper functions
member :: Code -> (Code, OrderConstant)
member code = (code, CMember)
none :: Code -> (Code, OrderConstant)
none code = (code, CNone)

type Code = String
type GeneratorFunction = Block -> Code

blockText :: GeneratorFunction
blockText block = member $ "text(\"" ++ arg ++ "\")"
  where
    arg = getFieldValue block "TEXT" 

-- TODO check if it is a number
blockNumber :: GeneratorFunction
blockNumber block = member arg 
  where
    arg = getFieldValue block "NUMBER"

blockDrawingOf :: GeneratorFunction
blockDrawingOf block = member $ "main = drawingOf(" ++ code ++ ");"
  where
    code = valueToCode block "VALUE" cORDER_ATOMIC

blockCombine :: GeneratorFunction
blockCombine block = none $ "(" ++ v1 ++ ") & (" ++ v2 ++ ")"
  where
    v1 = valueToCode block "Comb1" cORDER_ATOMIC
    v2 = valueToCode block "Comb2" cORDER_ATOMIC

blockTranslate :: GeneratorFunction
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

-- Assigns CodeGen functions defined here to the Blockly Javascript Code
-- generator
assignAll :: IO ()
assignAll = mapM (uncurry setCodeGen) blockCodeMap


valueToCode :: Block -> String -> Int -> String
valueToCode block name order = unpack $ js_valueToCode block (pack name) order



--- FFI
foreign import javascript unsafe "Blockly.FunBlocks[$1] = $2"
  js_setGenFunction :: JSString -> Callback a -> IO ()


foreign import javascript unsafe "Blockly.FunBlocks.valueToCode($1, $2, $3)"
  js_valueToCode :: Block -> JSString -> Int -> JSString

-- TODO, fix, Ugly hack incoming
foreign import javascript unsafe "[$1,$2]"
  js_makeArray :: JSString -> Int -> JSVal


-- TODO, remove, was used for testing
alert :: String -> IO ()
alert text = js_alert $ pack text
foreign import javascript unsafe "alert($1)" js_alert :: JSString -> IO ()



data OrderConstant =  CAtomic
                    | CMember
                    | CNew
                    | CFunctionCall
                    | CIncrement
                    | CDecrement
                    | CLogicalNot
                    | CBitwiseNot
                    | CUnaryPlus
                    | CUnaryNegation
                    | CTypeOf
                    | CVoid
                    | CDelete
                    | CMultiplication
                    | CDivision
                    | CModulus
                    | CAddition
                    | CSubstraction
                    | CBitwiseShift
                    | CRelational
                    | CIn
                    | CInstanceOf
                    | CEquality
                    | CBitwiseAnd
                    | CBitwiseXOR
                    | CBitwiseOR
                    | CLogicalAnd
                    | CLogicalOr
                    | CConditional
                    | CAssignment
                    | CComma
                    | CNone          


-- TODO, still JavaScript CodeGen stuff
order :: OrderConstant -> Int
order CAtomic         = 0;  -- 0 "" ...
order CMember         = 1;  -- . []
order CNew            = 1;  -- new
order CFunctionCall   = 2;  -- ()
order CIncrement      = 3;  -- ++
order CDecrement      = 3;  -- --
order CLogicalNot     = 4;  -- !
order CBitwiseNot     = 4;  -- ~
order CUnaryPlus      = 4;  -- +
order CUnaryNegation  = 4;  -- -
order CTypeOf         = 4;  -- typeof
order CVoid           = 4;  -- void
order CDelete         = 4;  -- delete
order CMultiplication = 5;  -- *
order CDivision       = 5;  -- /
order CModulus        = 5;  -- %
order CAddition       = 6;  -- +
order CSubstraction   = 6;  -- -
order CBitwiseShift   = 7;  -- << >> >>>
order CRelational     = 8;  -- < <= > >=
order CIn             = 8;  -- in
order CInstanceOf     = 8;  -- instanceof
order CEquality       = 9;  -- == != === !==
order CBitwiseAnd     = 10; -- &
order CBitwiseXOR     = 11; -- ^
order CBitwiseOR      = 12; -- |
order CLogicalAnd     = 13; -- &&
order CLogicalOr      = 14; -- ||
order CConditional    = 15; -- ?:
order CAssignment     = 16; -- = += -= *= /= %= <<= >>= ...
order CComma          = 17; -- ,
order CNone           = 99; -- (...)
















