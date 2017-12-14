import Test.Tasty --hiding (defaultMain)
--import Test.Tasty.Silver.Interactive
import qualified CAR.FillMetadata.Tests as FillMetadata

main = defaultMain $ testGroup "mediawiki-annotate" [
    FillMetadata.tests
  ]