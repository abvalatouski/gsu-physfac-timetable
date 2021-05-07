-- NOTE:
-- All the HTML from the site is valid XML, so I'll use `xml-conduit` package.
module Gsu.Physfac.Site.Parser
  where

import           Data.Functor
import           Data.Void

import           Control.Monad.Trans.Resource (ResIO, runResourceT)
import           Data.Conduit                 (ConduitT, runConduit, (.|))
import qualified Data.Conduit                 as Conduit
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.XML.Types               as Xml
import qualified Network.HTTP.Simple          as Net
import qualified Text.XML.Stream.Parse        as Xml

-- Type wrapper for convenient usage.
type Parser = ConduitT Xml.Event Void ResIO

fetch :: Parser a -> String -> IO a
fetch parser url = do
    request <- Net.parseRequestThrow url -- Forces the client to throw an exception on non-2XX.
    runResourceT $ runConduit $
        Net.httpSource request Net.getResponseBody
     .| Xml.parseBytes Xml.def
     .| parser

-- Useful combinators.
--
-- Convention:
-- - name  - required tag;
-- - name_ - ignored tag;
-- - name' - optional tag.

tag' :: Text -> Parser inner -> Parser (Maybe inner)
tag' = Xml.tagIgnoreAttrs . matchTagName

tag :: Text -> Parser inner -> Parser inner
tag name = Xml.force errorMessage . tag' name
  where
    errorMessage = "Expected <" <> Text.unpack name <> ">."

tag_ :: Text -> Parser ()
tag_ = void . flip Xml.ignoreTree Xml.ignoreAttrs . matchTagName

html :: Parser inner -> Parser inner
html = tag "html"

head_ :: Parser ()
head_ = tag_ "head"

body :: Parser inner -> Parser inner
body = tag "body"

div_ :: Parser ()
div_ = tag_ "div"

div :: Parser inner -> Parser inner
div = tag "div"

h3_ :: Parser ()
h3_ = tag_ "h3"

table_ :: Parser ()
table_ = tag_ "table"

table :: Parser inner -> Parser inner
table = tag "table"

tbody :: Parser inner -> Parser inner
tbody = tag "tbody"

tr_ :: Parser ()
tr_ = tag_ "tr"

tr :: Parser inner -> Parser inner
tr = tag "tr"

tr' :: Parser inner -> Parser (Maybe inner)
tr' = tag' "tr"

td :: Parser inner -> Parser inner
td = tag "td"

td' :: Parser inner -> Parser (Maybe inner)
td' = tag' "td"

span :: Parser inner -> Parser inner
span = tag "span"

span_ :: Parser ()
span_ = tag_ "span"

hr_ :: Parser ()
hr_ = tag_ "hr"

strong :: Parser inner -> Parser inner
strong = tag "strong"

-- Utils.

wrapTagName :: Text -> Xml.Name
wrapTagName name = Xml.Name name namespace prefix
  where
    namespace = Just "http://www.w3.org/1999/xhtml"
    prefix    = Nothing

matchTagName :: Text -> Xml.NameMatcher Xml.Name
matchTagName = Xml.matching . (==) . wrapTagName

-- Not the best way to handle errors, but now that's enough.
insertMissingEndTag :: Text -> Parser ()
insertMissingEndTag = Conduit.leftover . Xml.EventEndElement . wrapTagName
