{-# LANGUAGE OverloadedStrings #-}
module FilterPred where

import Control.Applicative
import Control.Monad (void)
import Data.Foldable
import Data.Hashable
import qualified Data.Text as T
import qualified Data.HashSet as HS

import Text.Trifecta
import Text.Parser.Expression
import Text.Parser.Token
import Text.Parser.Token.Style

import CAR.Types
import CAR.Utils

data Pred = NameContains T.Text
          | NameInSet (HS.HashSet PageName)
          | HasCategoryContaining T.Text
          | PageHashMod Int Int -- ^ for training/test split
          | IsRedirect

          | Any [Pred]
          | All [Pred]
          | Negate Pred
          deriving (Show)

pred :: Parser Pred
pred = term
  where
    opTable :: OperatorTable Parser Pred
    opTable = [ [ prefix "!" Negate ]
              , [ binary "&"   (\x y -> All [x,y]) AssocLeft
                , binary "and" (\x y -> All [x,y]) AssocLeft
                , binary "|"   (\x y -> Any [x,y]) AssocLeft
                , binary "or"  (\x y -> Any [x,y]) AssocLeft
                ]
              ]
      where
        binary  name fun assoc = Infix (fun <$ reservedOp name) assoc
        prefix  name fun       = Prefix (fun <$ reservedOp name)
        reservedOp name = reserve emptyOps name

    expr = buildExpressionParser opTable term <?> "job match expression"

    term = parens expr <|> simple

    simple =
        asum [ nameContains, nameInSet, hasCategoryContaining, pageHashMod
             , testSet, trainSet, isRedirect
             ]

    trainSet = textSymbol "train-set" >> pure (PageHashMod 2 0)
    testSet  = textSymbol "test-set"  >> pure (PageHashMod 2 1)
    isRedirect = textSymbol "is-redirect" >> pure IsRedirect

    nameContains = do
        textSymbol "name-contains"
        NameContains . T.toCaseFold <$> string

    nameInSet = do
        textSymbol "name-in-set"
        NameInSet . HS.fromList . map PageName <$> listOf string

    hasCategoryContaining = do
        void $ textSymbol "category-contains"
        HasCategoryContaining . T.toCaseFold <$> string

    pageHashMod = do
        textSymbol "page-hash"
        let natural' = fmap fromIntegral natural
        PageHashMod <$> natural' <*> natural'

    listOf :: Parser a -> Parser [a]
    listOf element = brackets $ commaSep element

    string :: Parser T.Text
    string = fmap T.pack $ stringLiteral <|> many alphaNum

normalize :: Pred -> Pred
normalize (Any [xs, Any ys]) = Any (xs:ys)
normalize (Any [Any xs, ys]) = Any (ys:xs)
normalize (All [xs, All ys]) = All (xs:ys)
normalize (All [All xs, ys]) = All (ys:xs)
normalize x                  = x

interpret :: Pred -> Page -> Bool
interpret (NameContains t)  page = t `T.isInfixOf` T.toCaseFold (getPageName (pageName page))
interpret (NameInSet names) page = pageName page `HS.member` names
interpret (HasCategoryContaining s) page =
    any (s `T.isInfixOf`) $ map T.toCaseFold $ pageCategories page
interpret (PageHashMod n m) page =
    let h = hash $ pageName page
    in h `mod` n == m
interpret  IsRedirect page = pageIsRedirect page
interpret (Any preds) page = any (`interpret` page) preds
interpret (All preds) page = all (`interpret` page) preds
interpret (Negate pred) page = not $ interpret pred page
