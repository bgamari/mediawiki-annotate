{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveTraversable #-}

module FilterPred where

import Data.Void
import Control.Applicative
import Control.Monad (void)
import Data.Foldable
import Data.Hashable
import qualified Data.Text as T
import qualified Data.HashSet as HS

import Text.Trifecta
import Text.Parser.Expression
import Text.Parser.Token.Style

import CAR.Types
import CAR.Utils

data Pred a = NameContains T.Text
            | NameHasPrefix T.Text
            | NameInSet (HS.HashSet PageName)
            | HasCategoryContaining T.Text
            | PageHashMod Int Int -- ^ for training/test split
            | IsRedirect

            | Any [Pred a]
            | All [Pred a]
            | Negate (Pred a)
            | Pure a
            deriving (Show, Functor, Foldable, Traversable)

runPred :: Applicative m => (a -> m (Pred b)) -> Pred a -> m (Pred b)
runPred _ (NameContains x)     = pure $ NameContains x
runPred _ (NameHasPrefix x)    = pure $ NameHasPrefix x
runPred _ (NameInSet x)        = pure $ NameInSet x
runPred _ (HasCategoryContaining x)    = pure $ HasCategoryContaining x
runPred _ (PageHashMod x y)    = pure $ PageHashMod x y
runPred _ IsRedirect    = pure IsRedirect
runPred f (Any x)     = Any <$> traverse (runPred f) x
runPred f (All x)     = All <$> traverse (runPred f) x
runPred f (Negate x)  = Negate <$> runPred f x
runPred f (Pure x)    = f x

pred :: Parser a -> Parser (Pred a)
pred inj = term
  where
    opTable :: OperatorTable Parser (Pred a)
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
             , testSet, trainSet, isRedirect, isDisambiguation
             , Pure <$> inj
             ]

    trainSet = textSymbol "train-set" >> pure (PageHashMod 2 0)
    testSet  = textSymbol "test-set"  >> pure (PageHashMod 2 1)
    isRedirect = textSymbol "is-redirect" >> pure IsRedirect
    isDisambiguation = do
        textSymbol "is-disambiguation"
        pure (NameContains " (disambiguation)")

    nameContains = do
        textSymbol "name-contains"
        NameContains . T.toCaseFold <$> string

    nameHasPrefix = do
        textSymbol "name-has-prefix"
        NameHasPrefix <$> string

    nameInSet = do
        textSymbol "name-in-set"
        NameInSet . HS.fromList . map PageName <$> listOf string

    hasCategoryContaining = do
        void $ textSymbol "category-contains"
        HasCategoryContaining . T.toCaseFold <$> string

    pageHashMod = do
        textSymbol "page-hash-mod"
        let natural' = fmap fromIntegral natural
        -- todo fail if k > n
        PageHashMod <$> natural' <*> natural'

    listOf :: Parser a -> Parser [a]
    listOf element = brackets $ commaSep element

    string :: Parser T.Text
    string = fmap T.pack $ stringLiteral <|> (many alphaNum <* whiteSpace)

-- todo fix this - currently not called, and does not apply recursively
normalize :: Pred a -> Pred a
normalize (Any [xs, Any ys]) = Any (xs:ys)
normalize (Any [Any xs, ys]) = Any (ys:xs)
normalize (All [xs, All ys]) = All (xs:ys)
normalize (All [All xs, ys]) = All (ys:xs)
normalize x                  = x

interpret :: Pred Void -> Page -> Bool
interpret (NameContains t)  page = t `T.isInfixOf` T.toCaseFold (getPageName (pageName page))
interpret (NameHasPrefix prefix) page = prefix `T.isPrefixOf` getPageName (pageName page)
interpret (NameInSet names) page = pageName page `HS.member` names
interpret (HasCategoryContaining s) page =
    any (s `T.isInfixOf`) $ map T.toCaseFold $ pageCategories page
interpret (PageHashMod n k) page =
    let h = hash $ pageName page
    in h `mod` n == k
interpret  IsRedirect page = pageIsRedirect page
interpret (Any preds) page = any (`interpret` page) preds
interpret (All preds) page = all (`interpret` page) preds
interpret (Negate pred) page = not $ interpret pred page
interpret (Pure _) _ = error "Impossible"
