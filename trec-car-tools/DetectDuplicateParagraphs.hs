{-# language TupleSections #-}

import CAR.AnnotationsFile
import CAR.CarExports
import CAR.Types
import qualified Data.HashMap.Strict as M

main :: IO ()
main = do
    anns <- openAnnotations "../data/enwiki-20161220/transformed-omit-pages.cbor"
    let m :: M.HashMap ParagraphId (Paragraph, Int, [PageId] )
        m = M.fromListWith (\(text,count1, pages1)  (_,  count2,pages2) -> (text, count1 + count2, pages1 ++ pages2))
            [ (paraId para, (para, 1::Int, [pageId page]))
            | page <- pages anns
            , para <- toParagraphs page
            ]
            -- $ map (\p -> (paraId p ,(p, 1::Int)))
            -- $ foldMap (\page -> [ (para, pageName) | para <- CAR.CarExports.toParagraphs page ]) $ pages anns
    let dups = filter (\(_,(_, n, _)) -> n > 10) $ M.toList m
    putStrLn $ unlines $ map show $ dups
