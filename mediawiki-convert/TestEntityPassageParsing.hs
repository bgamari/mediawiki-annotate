import Data.Proxy
import Control.Monad

import qualified Data.HashSet as HS
import Options.Applicative
import Codec.Serialise

import Control.Exception
import Data.Ord
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Text.Regex as TR

import Debug.Trace

import CAR.Types
import CAR.ToolVersion

args :: Parser String
args = argument str (metavar "STR")
main :: IO ()
main = do
    input <- execParser' 2 (helper <*> args) mempty
    print $ parsing $ T.pack input

parsing :: T.Text -> (T.Text, T.Text)
parsing docName = (T.pack a, b)
      where (a,b) =  case TR.matchRegex (TR.mkRegex "^([0-9a-f]+/)") (T.unpack docName) of
                        Nothing -> trace "nothing" $ ("", docName)
                        Just [] -> trace ("no matches in " <> (show docName)) $ ("", docName)
                        Just [ paraSlash ] ->
                                        let para = case T.unsnoc $ T.pack paraSlash of
                                                    Just (para, _) -> trace ("justpara " <> show para) $ para
                                                    Nothing -> T.pack $ trace ("parsePassageEntity: Issues with dropping slash of paraSlash " <> (show paraSlash)) $ ""
                                            entity = T.pack $ drop (length paraSlash) (T.unpack docName)
                                        in (T.unpack para, entity)
                        xx -> trace ("parsePassageEntity: Multiple para matches "<> (show docName) <> " : "++ (show xx)) $ ("", T.pack "")
