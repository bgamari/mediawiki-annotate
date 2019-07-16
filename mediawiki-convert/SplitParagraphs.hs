{-# LANGUAGE OverloadedStrings #-}

import Data.List.Split

import Options.Applicative

import CAR.Types.Files as CAR



options :: Parser (FilePath, FilePath, Int)
options =
    (,,)
      <$> argument str (help "paragraphs file" <> metavar "FILE" <> help "paragraphs.cbor file")
      <*> option str (short 'o' <> long "output-dir" <> metavar "FILE" <> help "Output directory")
      <*> option auto (short 'n' <> long "num-paras" <> metavar "N" <> help "Number of paragraphs per split")

main :: IO ()
main = do
    (paragraphsFile, outputPrefix, numParas) <- execParser $ info (helper <*> options) mempty
--     unprocessedPages <- openAnnotations unprocessedPagesFile
    (prov, paragraphs) <- CAR.readParagraphsFileWithProvenance paragraphsFile
    let listParas = zip [1:: Int ..] $ chunksOf numParas paragraphs
    mapM_ (writeChunk outputPrefix prov) $ listParas
  where writeChunk outputPrefix prov (fold, paras)=
            CAR.writeCarFile (outputPrefix<>"-"<> (show fold)<>".cbor") prov paras