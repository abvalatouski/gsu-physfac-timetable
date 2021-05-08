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

optionalTagIgnoreAttrs :: Text -> Parser inner -> Parser (Maybe inner)
optionalTagIgnoreAttrs = Xml.tagIgnoreAttrs . matchTagName

tagIgnoreAttrs :: Text -> Parser inner -> Parser inner
tagIgnoreAttrs name = Xml.force errorMessage . optionalTagIgnoreAttrs name
  where
    errorMessage = "Expected <" <> Text.unpack name <> ">."

tagIgnoreAttrs_ :: Text -> Parser ()
tagIgnoreAttrs_ = void . flip Xml.ignoreTree Xml.ignoreAttrs . matchTagName

optionalTag :: Text -> Xml.AttrParser attrs -> (attrs -> Parser inner) -> Parser (Maybe inner)
optionalTag = Xml.tag' . matchTagName

tag :: Text -> Xml.AttrParser attrs -> (attrs -> Parser inner) -> Parser inner
tag name attrs = Xml.force errorMessage . optionalTag name attrs
  where
    errorMessage = "Expected <" <> Text.unpack name <> ">."

html :: Parser inner -> Parser inner
html = tagIgnoreAttrs "html"

head_ :: Parser ()
head_ = tagIgnoreAttrs_ "head"

body :: Parser inner -> Parser inner
body = tagIgnoreAttrs "body"

div_ :: Parser ()
div_ = tagIgnoreAttrs_ "div"

div :: Parser inner -> Parser inner
div = tagIgnoreAttrs "div"

div' :: Parser inner -> Parser (Maybe inner)
div' = optionalTagIgnoreAttrs "div"

h3_ :: Parser ()
h3_ = tagIgnoreAttrs_ "h3"

h3 :: Parser inner -> Parser inner
h3 = tagIgnoreAttrs "h3"

table_ :: Parser ()
table_ = tagIgnoreAttrs_ "table"

table :: Parser inner -> Parser inner
table = tagIgnoreAttrs "table"

tbody :: Parser inner -> Parser inner
tbody = tagIgnoreAttrs "tbody"

tr_ :: Parser ()
tr_ = tagIgnoreAttrs_ "tr"

tr :: Parser inner -> Parser inner
tr = tagIgnoreAttrs "tr"

tr' :: Parser inner -> Parser (Maybe inner)
tr' = optionalTagIgnoreAttrs "tr"

td :: Parser inner -> Parser inner
td = tagIgnoreAttrs "td"

td' :: Parser inner -> Parser (Maybe inner)
td' = optionalTagIgnoreAttrs "td"

span :: Parser inner -> Parser inner
span = tagIgnoreAttrs "span"

span_ :: Parser ()
span_ = tagIgnoreAttrs_ "span"

hr_ :: Parser ()
hr_ = tagIgnoreAttrs_ "hr"

strong :: Parser inner -> Parser inner
strong = tagIgnoreAttrs "strong"

strong_ :: Parser ()
strong_ = tagIgnoreAttrs_ "strong"

script_ :: Parser ()
script_ = tagIgnoreAttrs_ "script"

a :: (Text -> Parser inner) -> Parser inner
a = tag "a" $ Xml.requireAttr "href" <* Xml.ignoreAttrs

img :: (Text -> Parser inner) -> Parser inner
img = tag "img" $ Xml.requireAttr "src" <* Xml.ignoreAttrs

img_ :: Parser ()
img_ = tagIgnoreAttrs_ "img"

h2 :: Parser inner -> Parser inner
h2 = tagIgnoreAttrs "h2"

dl_ :: Parser ()
dl_ = tagIgnoreAttrs_ "dl"

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
