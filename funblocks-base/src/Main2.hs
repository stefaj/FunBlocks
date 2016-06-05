-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (
    main
) where

import Control.Applicative ((<$>))
import GHCJS.DOM
       (enableInspector, webViewGetDomDocument, runWebGUI, currentDocument, )
import GHCJS.DOM.Document (getBody, createElement,getElementById, createTextNode, Document(..))
-- import GHCJS.DOM.HTMLButtonElement (onClick)
import GHCJS.DOM.Element (setInnerHTML,setId, click, Element)
import GHCJS.DOM.Node (appendChild)
import GHCJS.DOM.EventM (on, mouseClientXY)
import GHCJS.DOM.Types (castToHTMLElement, castToHTMLButtonElement)
import qualified CodeWorld as CW
import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal
import Data.JSString (unpack, pack)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Trans (liftIO)
import CodeGen
data Type = TNumber | TString | TPicture | TNone
  deriving Show



foreign import javascript unsafe "Blockly.inject($1, { toolbox: document.getElementById($2)})"
  js_blocklyInject :: JSString -> JSString -> IO Element

foreign import javascript unsafe "Blockly.JavaScript.workspaceToCode($1)"
  js_blocklyWorkspaceToCode :: Element -> IO JSString

--btnBoomClick :: Document -> Element -> IO ()
btnBoomClick doc ws = do
        Just body <- getBody doc
        -- (x, y) <- mouseClientXY
        Just newParagraph <- createElement doc (Just "p")
        jstext <- liftIO $ js_blocklyWorkspaceToCode ws
        text <- createTextNode doc $ unpack jstext
        appendChild newParagraph text
        appendChild body (Just newParagraph)
        return ()



main = do 
    
      Just doc <- currentDocument 
      Just body <- getBody doc

      ws :: Element <- js_blocklyInject (pack "blocklyCanvas") (pack "toolbox")

      Just btnBoom <- getElementById doc "btnBoom"  -- fmap castToHTMLButtonElement <$> getElementById doc "btnBoom"
      on btnBoom click (btnBoomClick doc ws)

      CW.drawingOf (CW.circle (5))



      return ()
    
-- main = runWebGUI $ \ webView -> do
--     enableInspector webView
--     Just doc <- webViewGetDomDocument webView
--     Just body <- getBody doc
--     setInnerHTML body (Just "<h1>Hello World</h1><div><button id='btnBoom'>test</button></div>")
--     Just btnBoom <- getElementById doc "btnBoom"  -- fmap castToHTMLButtonElement <$> getElementById doc "btnBoom"
--     on btnBoom click (btnBoomClick doc) 
--     return ()
