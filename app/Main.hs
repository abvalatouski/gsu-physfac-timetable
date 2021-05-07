import           Control.Monad

import           Data.Aeson                  (ToJSON)
import qualified Data.Aeson                  as Json
import qualified Data.Aeson.Encode.Pretty    as Json
import qualified Data.ByteString.Lazy        as Lbs
import           System.Directory

import           Gsu.Physfac.Site.Pages.Home
import           Gsu.Physfac.Site.Parser

main :: IO ()
main = do
    let url = "https://old.gsu.by/physfac/"
    HomePage bellRings weekPairs <- fetch homePage url

    exists <- doesDirectoryExist "out"
    unless exists $
        createDirectory "out"

    saveAsJson SavePretty "out/Расписание звонков.json" bellRings
    saveAsJson SavePretty "out/Недели над и под чертой.json" weekPairs

data SaveMode = Save | SavePretty

saveAsJson :: ToJSON a => SaveMode -> FilePath -> a -> IO ()
saveAsJson Save       path object = Json.encodeFile path object
saveAsJson SavePretty path object = Lbs.writeFile path $ Json.encodePretty' settings object
  where
    settings = Json.defConfig { Json.confTrailingNewline = True }
