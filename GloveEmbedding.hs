{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}

module GloveEmbedding
    ( EmbeddingDim
    , WordVec
    , unWordVec
    , wordVecDim
    , WordEmbedding
    , SomeWordEmbedding(..)
    , someWordEmbeddingDim
    , wordEmbeddingDim
    , parseGlove
    , parseGlove'
    ) where

import Data.Proxy
import Data.Ix
import Data.Semigroup
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Read as TR
import qualified Data.HashMap.Strict as HM
import qualified Data.Array.Unboxed as A
import GHC.TypeLits

-- | A embedding dimension index.
newtype EmbeddingDim n = EmbeddingDim Int
                 deriving (Show, Eq, Ord, Ix)

-- | A embedding word vector.
newtype WordVec (n :: Nat) = WordVec { unWordVec :: A.UArray (EmbeddingDim n) Float }

-- | The dimension of a word vector.
wordVecDim :: WordVec n -> Int
wordVecDim = rangeSize . A.bounds . unWordVec

bounds :: forall (n :: Nat). KnownNat n => (EmbeddingDim n, EmbeddingDim n)
bounds = (EmbeddingDim 0, EmbeddingDim $ fromIntegral (natVal (Proxy @n)) - 1)

instance KnownNat n => Monoid (WordVec n) where
    mempty = WordVec $ A.accumArray (+) 0 (bounds @n) []
    mappend = (<>)
    mconcat xs =
        WordVec $ A.accumArray (+) 0 (bounds @n)
        [ (i, v / nWords)
        | WordVec vec <- xs
        , (i, v) <- A.assocs vec
        ]
      where
        !nWords = realToFrac $ length xs

-- | Word vector addition.
instance KnownNat n => Semigroup (WordVec n) where
    WordVec a <> WordVec b =
        WordVec $ A.listArray (bounds @n) (zipWith avg (A.elems a) (A.elems b))
      where
        avg x y = (x+y) / 2

    sconcat (x :| xs) = mconcat (x:xs)
    stimes _ x = x

-- | A embedding word embedding.
type WordEmbedding n = HM.HashMap T.Text (WordVec n)

data SomeWordEmbedding where
    SomeWordEmbedding :: KnownNat n => !(WordEmbedding n) -> SomeWordEmbedding

someWordEmbeddingDim :: SomeWordEmbedding -> Int
someWordEmbeddingDim (SomeWordEmbedding d) = wordEmbeddingDim d

-- | The dimension of a word embedding
wordEmbeddingDim :: forall n. KnownNat n => WordEmbedding n -> Int
wordEmbeddingDim _ = fromIntegral $ natVal (Proxy @n)

-- | Parse GloVe word embeddings from file.
parseGlove :: FilePath -> IO SomeWordEmbedding
parseGlove path = parseGlove' <$> TL.readFile path

-- | Parse GloVe word embeddings.
parseGlove' :: TL.Text -> SomeWordEmbedding
parseGlove' contents =
    case someNatVal $ fromIntegral dim of
      Just (SomeNat (Proxy :: Proxy n)) ->
          SomeWordEmbedding $
              let vecs :: KnownNat n => WordEmbedding n
                  vecs = mconcat $ map parse $ TL.lines contents
              in vecs
      Nothing -> error "parseGlove': impossible"
  where
    !dim | x:_ <- TL.lines contents = length (TL.words x) - 1
         | otherwise = error "GloveEmbedding.parseGlove': Empty embeddings"
    dimRange = (EmbeddingDim 0, EmbeddingDim (dim-1))

    parse :: KnownNat n => TL.Text -> HM.HashMap T.Text (WordVec n)
    parse line =
      case TL.words line of
        [] -> mempty
        w : ws -> HM.singleton (TL.toStrict w)
                               (WordVec $ A.listArray dimRange $ map parseFloat ws)

    parseFloat :: TL.Text -> Float
    parseFloat = either err (realToFrac . fst) . TR.double . TL.toStrict
      where err msg = error $ "GloveEmbedding.parseGlove: parse error: "++msg