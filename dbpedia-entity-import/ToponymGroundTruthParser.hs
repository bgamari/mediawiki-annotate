
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module ToponymGroundTruthParser where

import Data.Maybe
import Data.Foldable
import qualified Data.Text as T
import Data.List (partition)
import Control.Applicative

import System.FilePath
import Text.Trifecta as Tri



type Offsets = (Int, Int)

loadGroundTruthFromFiles :: [FilePath] -> IO [(T.Text, ([Offsets], [Offsets]))]
loadGroundTruthFromFiles (f1:rest) = do
    d1 <- loadGroundTruthFromFile f1
    drest <- loadGroundTruthFromFiles rest
    return $ d1:drest
loadGroundTruthFromFiles [] = do
    pure []



data GroundTruthEntry = GroundTruthEntry { labelClass :: String
                                         , labelOffsets :: Offsets
                                         , labelEntity :: String
                                         }


loadGroundTruthFromFile :: FilePath -> IO (T.Text, ([Offsets], [Offsets]))
loadGroundTruthFromFile fname = do
    let filename = T.pack $ takeBaseName fname
        -- posOffsets: T39     Location 20830 20844    United Kingdom
        -- negOffsets: T23     Protein 29534 29545     Gestational

    Just truthEntries <- parseFromFile groundTruthParser fname
                       :: IO (Maybe [GroundTruthEntry])
    let (posEntries, negEntries) = partition (\entry -> labelClass entry == "Location") truthEntries
    return (filename, (map labelOffsets posEntries, map labelOffsets negEntries))
  where
    groundTruthParser :: Parser [GroundTruthEntry]
    groundTruthParser = do
        res <- fmap catMaybes $ many (comment <|> entry)
        eof
        return res

    comment = do
        char '#'
        many $ notChar '\n'
        newline
        return Nothing

    entry = do
        many $ notChar '\t'
        char '\t'

        labelClass <- many alphaNum
        char ' '
        labelOffsetStart <- fromIntegral <$> integer
        labelOffsetEnd <- fromIntegral <$> integer

        labelEntity <- many $ notChar '\n'
        newline

        return $ Just $ GroundTruthEntry labelClass (labelOffsetStart, labelOffsetEnd) labelEntity

