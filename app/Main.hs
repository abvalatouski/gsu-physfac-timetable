import           Control.Monad
import           Data.Maybe

import           Control.Monad.Extra               (whenJust)
import           Data.Aeson                        (ToJSON)
import qualified Data.Aeson                        as Json
import qualified Data.Aeson.Encode.Pretty          as Json
import qualified Data.ByteString.Lazy              as Lbs

import           Gsu.Physfac.Site.Pages.Home
import           Gsu.Physfac.Site.Parser
import           Options

main :: IO ()
main = do
    Options { .. } <- parseOptions
    when (any isJust [optionsWeeks, optionsBellRings]) do
        let url = "https://old.gsu.by/physfac/"
        HomePage bellRings weekPairs <- fetch homePage url

        whenJust optionsWeeks \(path, saveMode) ->
            saveAsJson saveMode path weekPairs

        whenJust optionsBellRings \(path, saveMode) ->
            saveAsJson saveMode path bellRings

-- parseRepository :: IO ()
-- parseRepository = do
--     let url = "https://old.gsu.by/physfac/index.php?option=com_remository&func=select&id=313"
--     RepositoryFile file _image _name : _ <- fetch repository url
-- 
--     let url = "https://old.gsu.by" <> Text.unpack file
--     printf "Fetching from:\n%s\n\n" url
--     link <- fetch downloadLink url
--     let url = "https://old.gsu.by" <> Text.unpack link
--     printf "Found a link:\n%s\n" url

saveAsJson :: ToJSON a => SaveMode -> FilePath -> a -> IO ()
saveAsJson Save       path object = Json.encodeFile path object
saveAsJson SavePretty path object = Lbs.writeFile path $ Json.encodePretty' settings object
  where
    settings = Json.defConfig { Json.confTrailingNewline = True }
