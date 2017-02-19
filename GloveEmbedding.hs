{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}

module GloveEmbedding
    ( GloveDim
    , WordVec
    , WordEmbedding
    , wordEmbeddingDim
    , parseGlove
    , parseGlove'
    ) where

import Data.Ix
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Read as TR
import qualified Data.HashMap.Strict as HM
import qualified Data.Array.Unboxed as A

-- | A GloVe dimension index.
newtype GloveDim = GloveDim Int
                 deriving (Show, Eq, Ord, Ix)

-- | A GloVe word vector.
type WordVec = A.UArray GloveDim Float

-- | A GloVe word embedding.
type WordEmbedding = HM.HashMap T.Text WordVec

wordEmbeddingDim :: WordEmbedding -> Int
wordEmbeddingDim = rangeSize . A.bounds . head . HM.elems

-- | Parse GloVe word embeddings from file.
parseGlove :: FilePath -> IO WordEmbedding
parseGlove path = parseGlove' <$> TL.readFile path

-- | Parse GloVe word embeddings.
parseGlove' :: TL.Text -> WordEmbedding
parseGlove' contents = mconcat $ map parse xs
  where
    xs = TL.lines contents
    !dim | x:_ <- xs = length (TL.words x) - 1
         | otherwise = error "GloveEmbedding.parseGlove': Empty embeddings"
    dimRange = (GloveDim 0, GloveDim (dim-1))

    parse :: TL.Text -> HM.HashMap T.Text WordVec
    parse line =
      case TL.words line of
        [] -> mempty
        w : ws -> HM.singleton (TL.toStrict w)
                               (A.listArray dimRange $ map parseFloat ws)

    parseFloat :: TL.Text -> Float
    parseFloat = either err (realToFrac . fst) . TR.double . TL.toStrict
      where err msg = error $ "GloveEmbedding.parseGlove: parse error: "++msg