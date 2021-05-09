module Gsu.Physfac.Timetable.Document.Xlsx
  where

import           Data.Char                                  hiding (digitToInt)
import           Data.Foldable                              hiding (or)
import           Data.Functor
import           Data.List                                  hiding (or)
import           Data.Maybe
import           Prelude                                    hiding (or)

import qualified Codec.Archive.Zip                          as Zip
import           Control.DeepSeq                            (NFData (rnf))
import qualified Data.ByteString.Lazy                       as Lazy (ByteString)
import           Data.Text                                  (Text)
import qualified Data.Text                                  as Text
import           Data.Vector                                (Vector)
import qualified Data.Vector                                as Vector
import qualified Text.XML.Stream.Parse                      as Xml

import           Gsu.Physfac.Common
import           Gsu.Physfac.Timetable.Document.Xlsx.Parser

-- XLSX implementation.

data Document = Document
    { documentSharedStringTable :: !SharedStringTable
    , documentWorksheet         :: !Worksheet
    }
  deriving stock Show

parseDocument :: Lazy.ByteString -> IO Document
parseDocument xlsx = do
    let (sharedStrings, sheet1) = unzipXmls $ Zip.toArchive xlsx
    Document <$> parseSharedStringTable sharedStrings <*> parseWorksheet sheet1

unzipXmls :: Zip.Archive -> (Lazy.ByteString, Lazy.ByteString)
unzipXmls zip = (unzip "xl/sharedStrings.xml", unzip "xl/worksheets/sheet1.xml")
  where
    unzip = Zip.fromEntry . fromJust . flip Zip.findEntryByPath zip

-- See Microsoft documentation:
-- https://docs.microsoft.com/en-us/office/open-xml/working-with-the-shared-string-table#shared-string-item-class
type SharedStringTable = Vector Text

parseSharedStringTable :: Lazy.ByteString -> IO SharedStringTable
parseSharedStringTable = parse $
    sst do
        fmap Vector.fromList $ many $ si' $
            Xml.force "Expected <t> or several <r>." $
                or
                    do
                        t' content
                    do
                        rs <- many $ r' $ rPr_ *> t content
                        pure $ Just $ mconcat rs

-- See Microsoft documentation:
-- https://docs.microsoft.com/en-us/office/open-xml/working-with-sheets
data Worksheet = Worksheet
    { worksheetSheetData  :: [[Cell Int]]
    , worksheetMergeCells :: [MergeCell]
    }
  deriving stock Show

parseWorksheet :: Lazy.ByteString -> IO Worksheet
parseWorksheet = parse $
    worksheet do
        dimension_
        sheetViews_
        sheetFormatPr_
        cols_
        worksheetSheetData <- sheetData do
            many $ row' do
                many $ c' \case
                    Just _cellType -> Cell . Just . toInt <$> v content
                    Nothing        -> pure $ Cell Nothing
        worksheetMergeCells <- mergeCells do
            many $ mergeCell' \ref -> do
                let [from, to] = fromA1Notation . Text.unpack <$> Text.splitOn ":" ref
                pure $ MergeCell from to
        phoneticPr_
        pageMargins_
        pageSetup_
        headerFooter_
        rowBreaks_
        colBreaks_
        pure Worksheet { .. }

newtype Cell a = Cell
    { getCell :: Maybe a
    }
  deriving stock   Eq
  deriving newtype (Functor, NFData)

instance Show a => Show (Cell a) where
    show (Cell cell) = maybe "-" show cell

data MergeCell = MergeCell !Location !Location
  deriving stock Show

instance NFData MergeCell where
    rnf (MergeCell from to) = rnf (from, to)

-- Utils.

data Location = Location !Int !Int
  deriving stock Show

instance NFData Location where
    rnf (Location row column) = rnf (row, column)

fromA1Notation :: String -> Location
fromA1Notation value =
    let row    = foldl' (\acc n -> digitToInt n + acc * 10) 0 digits
        column = columnToInt letters
     in Location row column
  where
    (letters, digits) = partition isAlpha value

digitToInt :: Char -> Int
digitToInt c = ord c - ord '0'

-- Stolen from:
-- https://hackage.haskell.org/package/xlsx-0.8.3/docs/src/Codec.Xlsx.Types.Common.html#col2int
columnToInt :: String -> Int
columnToInt column =
    let n = foldl' (\i acc -> i * length ['A' .. 'Z'] + ord acc - ord 'A' + 1) 0 column
     in n - 1 -- Programmers enumerate everything starting from zero.
