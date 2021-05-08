import           Control.Monad
import           Data.Maybe

import           Control.Monad.Extra               (whenJust)
import           Data.Aeson                        (ToJSON)
import qualified Data.Aeson                        as Json
import qualified Data.Aeson.Encode.Pretty          as Json
import qualified Data.ByteString.Lazy              as Lbs
import qualified Data.Text                         as Text
import qualified Network.HTTP.Simple               as Net

import           Gsu.Physfac.Site.Pages.Download
import           Gsu.Physfac.Site.Pages.Home
import           Gsu.Physfac.Site.Pages.Repository
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

    whenJust optionsTimetable \(_path, _saveMode) -> do
        -- TODO: Find that link dynamicly.
        let url = "https://old.gsu.by/physfac/index.php?option=com_remository&func=select&id=313"
        RepositoryFile file _image _name : _ <- fetch repository url

        let url = "https://old.gsu.by" <> Text.unpack file
        link <- fetch downloadLink url

        let url     = "https://old.gsu.by" <> Text.unpack link
            request = Net.parseRequestThrow_ url
        document <- Net.getResponseBody <$> Net.httpLBS request

        pure ()

saveAsJson :: ToJSON a => SaveMode -> FilePath -> a -> IO ()
saveAsJson Save       path object = Json.encodeFile path object
saveAsJson SavePretty path object = Lbs.writeFile path $ Json.encodePretty' settings object
  where
    settings = Json.defConfig { Json.confTrailingNewline = True }
