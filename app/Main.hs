import           Control.Monad
import           Text.Printf

import           Data.Aeson                        (ToJSON)
import qualified Data.Aeson                        as Json
import qualified Data.Aeson.Encode.Pretty          as Json
import qualified Data.ByteString.Lazy              as Lbs
import qualified Data.Text                         as Text
import           System.Directory

import           Gsu.Physfac.Site.Pages.Download
import           Gsu.Physfac.Site.Pages.Home
import           Gsu.Physfac.Site.Pages.Repository
import           Gsu.Physfac.Site.Parser

main :: IO ()
main = do
    exists <- doesDirectoryExist "out"
    unless exists $
        createDirectory "out"
    
    parseHome
    -- parseRepository

parseHome :: IO ()
parseHome = do
    let url = "https://old.gsu.by/physfac/"
    HomePage bellRings weekPairs <- fetch homePage url

    saveAsJson SavePretty "out/Расписание звонков.json" bellRings
    saveAsJson SavePretty "out/Недели над и под чертой.json" weekPairs

parseRepository :: IO ()
parseRepository = do
    let url = "https://old.gsu.by/physfac/index.php?option=com_remository&func=select&id=313"
    RepositoryFile file _image _name : _ <- fetch repository url

    let url = "https://old.gsu.by" <> Text.unpack file
    printf "Fetching from:\n%s\n\n" url
    link <- fetch downloadLink url
    let url = "https://old.gsu.by" <> Text.unpack link
    printf "Found a link:\n%s\n" url

data SaveMode = Save | SavePretty

saveAsJson :: ToJSON a => SaveMode -> FilePath -> a -> IO ()
saveAsJson Save       path object = Json.encodeFile path object
saveAsJson SavePretty path object = Lbs.writeFile path $ Json.encodePretty' settings object
  where
    settings = Json.defConfig { Json.confTrailingNewline = True }
