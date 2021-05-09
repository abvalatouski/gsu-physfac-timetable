module Gsu.Physfac.Timetable.Document.ShrinkToFit
  where

import           Control.DeepSeq                     (NFData (rnf), force)
import qualified Data.ByteString.Lazy                as Lazy (ByteString)
import           Data.Text                           (Text)
import           Data.Vector                         (Vector, (!))
import qualified Data.Vector                         as Vector

import           Gsu.Physfac.Timetable.Document.Xlsx

data Table = Table
    { tableWidth      :: !Int
    , tableHeight     :: !Int
    , tableCells      :: !(Vector (Cell Text))
    , tableMergeCells :: !(Vector MergeCell)
    }
  deriving stock Show

instance NFData Table where
    rnf (Table width height cells mergeCells) = rnf (width, height, cells, mergeCells)

cellAt :: Int -> Int -> Table -> Cell Text
cellAt row column (Table width _ cells _) =
    let index = row * width + column
     in cells ! index

parseTable :: Lazy.ByteString -> IO Table
parseTable xlsx = do
    Document sharedStrings (Worksheet sheetData mergeCells) <- parseDocument xlsx
    let width       = estimateContentWidth sheetData
        height      = contentHeight
        cells       = packCells $ substituteSharedStrings sharedStrings $ shrinkSheetData sheetData
        mergeCells' = Vector.fromList $ map (stripMergeCell width height) mergeCells
    pure $ force $ Table width height cells mergeCells'

-- Estimate size of the table contents.

estimateContentWidth :: [[Cell Int]] -> Int
estimateContentWidth cells =
    let rowIndex  = 4 - 1 -- 4th row starts and ends with "Monday".
        row       = cells !! rowIndex
        mondayId  = head row
        isEnd     = (== mondayId)
        skipped   = 2
        remaining = 2 -- `takeWhile` doesn't take into account monday itself and cell behind it.
     in skipped + length (takeWhile (not . isEnd) $ drop skipped row) + remaining

contentHeight :: Int
contentHeight =
    header + years + groups + days * classes * rowsPerClass + years + groups
  where
    header       = 1
    years        = 1
    groups       = 1
    days         = 6
    classes      = 6
    rowsPerClass = 2

shrinkSheetData :: [[Cell Int]] -> [[Cell Int]]
shrinkSheetData cells =
    let width = estimateContentWidth cells
     in shortenContentRows $ fmap (take width) cells

shortenContentRows :: [[Cell Int]] -> [[Cell Int]]
shortenContentRows = take contentHeight

stripMergeCell :: Int -> Int -> MergeCell -> MergeCell
stripMergeCell width height (MergeCell (Location y1 x1) (Location y2 x2)) =
    let y1' = min (height - 1) y1
        x1' = min (width  - 1) x1
        y2' = min (height - 1) y2
        x2' = min (width  - 1) x2
     in MergeCell (Location y1' x1') (Location y2' x2')

packCells :: [[Cell Text]] -> Vector (Cell Text)
packCells = Vector.fromList . concat

substituteSharedStrings :: SharedStringTable -> [[Cell Int]] -> [[Cell Text]]
substituteSharedStrings strings = fmap (fmap substitute)
  where
    substitute = fmap (strings !)
