module Main where

import GHCJS.DOM
import GHCJS.DOM.Window
import GHCJS.DOM.Document
import GHCJS.DOM.Node

main = do
    runWebGUI ( \ webview -> do
                    Just doc <- webViewGetDomDocument webview
                    Just id <- documentGetElementById doc "my_id"
                    value <- nodeGetNodeValue id
                    putStrLn value
              )