module CAR.ToolVersion
  ( versionedTool
  , execParser'
  ) where

import Data.Semigroup
import Options.Applicative
import System.Exit

type ToolVersion = Int

versionedToolFlagFields :: Mod FlagFields a
versionedToolFlagFields = long "tool-version" <> help "print the version of this CAR tool"

versionedTool :: ToolVersion -> Parser (IO () -> IO ())
versionedTool version = f <$> switch versionedToolFlagFields
  where
    f True  = const $ printVersion version
    f False = id

printVersion :: ToolVersion -> IO ()
printVersion = print

execParser' :: ToolVersion -> Parser a -> InfoMod (Maybe a) -> IO a
execParser' version parser pinfo = do
    x <- execParser $ info (flag' Nothing versionedToolFlagFields <|> fmap Just parser) pinfo
    maybe (printVersion version >> exitSuccess) pure x
