{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe
import Snap.Util.FileUploads
import System.Directory
import System.FilePath

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
        ifTop (writeBS "hellow world") <|>
        route [ ("foo", writeBS "bar")
               ,("echo/:echoparam", echoHandler)
               ] <|>
        serveDirectory "web"


echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echoparam in URL")
          writeBS param
