{-# LANGUAGE OverloadedStrings #-}
module AnnotationExport where


import Control.Monad
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!), p, ul, li, toHtml)
import Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Utf8 as H
import qualified Data.ByteString.Lazy as BSL

numbers :: Int -> H.Html
numbers n = H.docTypeHtml $ do
  H.head $ do
      H.title "Natural numbers"

  H.body ! class_ "a-class" $ do
      p "A list of natural numbers:"
      ul $ forM_ [1 .. n] (li . toHtml)



main :: IO ()

main = BSL.putStr $ H.renderHtml $ numbers 5
