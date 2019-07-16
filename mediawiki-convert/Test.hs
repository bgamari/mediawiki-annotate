import Data.Proxy
import Test.Tasty --hiding (defaultMain)
import Test.Tasty.Options
--import Test.Tasty.Silver.Interactive
import qualified CAR.FillMetadata.Tests as FillMetadata

main = defaultMainWithIngredients
    ([includingOptions [Option (Proxy :: Proxy FillMetadata.LargeTestDataPath)]] ++ defaultIngredients)
    $ testGroup "mediawiki-annotate" [
        FillMetadata.tests
      ]