module Main (
    main
) where

import Control.Applicative ((<$>))
import GHCJS.DOM
       (enableInspector, webViewGetDomDocument, runWebGUI)
import GHCJS.DOM.Document (getBody, createElement,getElementById, createTextNode)
-- import GHCJS.DOM.HTMLButtonElement (onClick)
import GHCJS.DOM.Element (setInnerHTML, click)
import GHCJS.DOM.Node (appendChild)
import GHCJS.DOM.EventM (on, mouseClientXY)
import GHCJS.DOM.Types (castToHTMLElement, castToHTMLButtonElement)

btnBoomClick doc = do
        Just body <- getBody doc
        -- (x, y) <- mouseClientXY
        Just newParagraph <- createElement doc (Just "p")
        text <- createTextNode doc $ "Click " -- ++ show (x, y)
        appendChild newParagraph text
        appendChild body (Just newParagraph)
        return ()



main = runWebGUI $ \ webView -> do
    enableInspector webView
    Just doc <- webViewGetDomDocument webView
    Just body <- getBody doc
    setInnerHTML body (Just "<h1>Hello World</h1><div><button id='btnBoom'>test</button></div>")
    Just btnBoom <- getElementById doc "btnBoom"  -- fmap castToHTMLButtonElement <$> getElementById doc "btnBoom"
    on btnBoom click (btnBoomClick doc) 
    return ()
