module Gsu.Physfac.Timetable.Document.Xlsx.Parser
  where

import           Control.Monad.Reader
import qualified Data.ByteString.Lazy  as Lazy (ByteString)
import           Data.Conduit          (runConduit, (.|))
import           Data.Text             (Text)
import qualified Text.XML.Stream.Parse as Xml

import           Gsu.Physfac.Common

-- Type wrapper for convenient usage.
type Parser = XmlParserT IO

parse :: Parser a -> Lazy.ByteString -> IO a
parse parser xml = runConduit $
    Xml.parseLBS Xml.def xml
 .| runReaderT parser (XmlSettings xmlNamespace xmlPrefix)
  where
    xmlNamespace = Just "http://schemas.openxmlformats.org/spreadsheetml/2006/main"
    xmlPrefix    = Nothing

-- Useful combinators.

sst :: Parser inner -> Parser inner
sst = tagIgnoreAttrs "sst"

si' :: Parser inner -> Parser (Maybe inner)
si' = optionalTagIgnoreAttrs "si"

t :: Parser inner -> Parser inner
t = tagIgnoreAttrs "t"

t' :: Parser inner -> Parser (Maybe inner)
t' = optionalTagIgnoreAttrs "t"

r' :: Parser inner -> Parser (Maybe inner)
r' = optionalTagIgnoreAttrs "r"

rPr_ :: Parser ()
rPr_ = tagIgnoreAttrs_ "rPr"

worksheet :: Parser inner -> Parser inner
worksheet = tagIgnoreAttrs "worksheet"

dimension_ :: Parser ()
dimension_ = tagIgnoreAttrs_ "dimension"

sheetViews_ :: Parser ()
sheetViews_ = tagIgnoreAttrs_ "sheetViews"

sheetFormatPr_ :: Parser ()
sheetFormatPr_ = tagIgnoreAttrs_ "sheetFormatPr"

cols_ :: Parser ()
cols_ = tagIgnoreAttrs_ "cols"

sheetData :: Parser inner -> Parser inner
sheetData = tagIgnoreAttrs "sheetData"

row' :: Parser inner -> Parser (Maybe inner)
row' = optionalTagIgnoreAttrs "row"

c' :: (Maybe Text -> Parser inner) -> Parser (Maybe inner)
c' = optionalTag "c" $ Xml.requireAttr "r" *> Xml.requireAttr "s" *> Xml.attr "t"

v :: Parser inner -> Parser inner
v = tagIgnoreAttrs "v"

mergeCells :: Parser inner -> Parser inner
mergeCells = tagIgnoreAttrs "mergeCells"

mergeCell' :: (Text -> Parser inner) -> Parser (Maybe inner)
mergeCell' = optionalTag "mergeCell" $ Xml.requireAttr "ref"

phoneticPr_ :: Parser ()
phoneticPr_ = tagIgnoreAttrs_ "phoneticPr"

pageMargins_ :: Parser ()
pageMargins_ = tagIgnoreAttrs_ "pageMargins"

pageSetup_ :: Parser ()
pageSetup_ =  tagIgnoreAttrs_ "pageSetup"

headerFooter_ :: Parser ()
headerFooter_ = tagIgnoreAttrs_ "headerFooter"

rowBreaks_ :: Parser ()
rowBreaks_ = tagIgnoreAttrs_ "rowBreaks"

colBreaks_ :: Parser ()
colBreaks_ = tagIgnoreAttrs_ "colBreaks"
