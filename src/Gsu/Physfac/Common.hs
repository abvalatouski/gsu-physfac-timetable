-- TODO: Split into different modules.
module Gsu.Physfac.Common
  where

import           Data.Void
import           GHC.Generics

import           Control.Monad.Catch
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Conduit          (ConduitT)
import qualified Data.Conduit          as Conduit
import           Data.Text             (Text)
import qualified Data.Text             as Text
import qualified Data.XML.Types        as Xml
import qualified Text.XML.Stream.Parse as Xml

-- Parsing XML.

type XmlParserT m = ReaderT XmlSettings (ConduitT Xml.Event Void m)

data XmlSettings = XmlSettings
    { xmlNamespace :: !(Maybe Text)
    , xmlPrefix    :: !(Maybe Text)
    }

content ::
    MonadThrow m
 => XmlParserT m Text
content = lift Xml.content

optionalTagIgnoreAttrs ::
    MonadThrow m
 => Text -> XmlParserT m inner -> XmlParserT m (Maybe inner)
optionalTagIgnoreAttrs name inner = do
    xmlSettings <- ask
    let name' = matchTagName xmlSettings name
    -- TODO: Try to refactor that ugly mess with `ReaderT`.
    lift $ Xml.tagIgnoreAttrs name' $
        runReaderT inner xmlSettings

tagIgnoreAttrs ::
    MonadThrow m
 => Text -> XmlParserT m inner -> XmlParserT m inner
tagIgnoreAttrs name = Xml.force errorMessage . optionalTagIgnoreAttrs name
  where
    errorMessage = "Expected <" <> Text.unpack name <> ">."

tagIgnoreAttrs_ ::
    MonadThrow m
 => Text -> XmlParserT m ()
tagIgnoreAttrs_ name = do
    xmlSettings <- ask
    let name' = matchTagName xmlSettings name
    void $ lift $ Xml.ignoreTree name' Xml.ignoreAttrs

optionalTag ::
    MonadThrow m
 => Text -> Xml.AttrParser attrs -> (attrs -> XmlParserT m inner) -> XmlParserT m (Maybe inner)
optionalTag name attrs inner = do
    xmlSettings <- ask
    let name' = matchTagName xmlSettings name
    -- TODO: Try to refactor that ugly mess with `ReaderT`.
    lift $ Xml.tag' name' attrs \attrs ->
        runReaderT (inner attrs) xmlSettings

tag ::
    MonadThrow m
 => Text -> Xml.AttrParser attrs -> (attrs -> XmlParserT m inner) -> XmlParserT m inner
tag name attrs = Xml.force errorMessage . optionalTag name attrs
  where
    errorMessage = "Expected <" <> Text.unpack name <> ">."

many ::
    MonadThrow m
 => XmlParserT m (Maybe a) -> XmlParserT m [a]
many item = do
    xmlSettings <- ask
    -- TODO: Try to refactor that ugly mess with `ReaderT`.
    lift $ Xml.many $ runReaderT item xmlSettings

wrapTagName :: XmlSettings -> Text -> Xml.Name
wrapTagName (XmlSettings namespace prefix) name = Xml.Name name namespace prefix

matchTagName :: XmlSettings -> Text -> Xml.NameMatcher Xml.Name
matchTagName settings = Xml.matching . (==) . wrapTagName settings

-- Not the best way to handle errors, but now that's enough.
insertMissingEndTag :: Text -> XmlParserT m ()
insertMissingEndTag name = do
    xmlSettings <- ask
    let name' = wrapTagName xmlSettings name
    lift $ Conduit.leftover $ Xml.EventEndElement name'

-- Representing data in JSON.

data HoursAndMinutes = HoursAndMinutes
    { hours   :: !Int
    , minutes :: !Int
    }
  deriving stock    (Show, Generic)
  deriving anyclass ToJSON
