module Options
  where

import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Maybe
import           System.Environment
import           System.Exit
import           System.IO

import qualified Options.Applicative as Options

-- How to save JSON data.
data SaveMode = Save | SavePretty
  deriving stock Show

data Options = Options
    { optionsWeeks     :: !(Maybe (FilePath, SaveMode))
    , optionsBellRings :: !(Maybe (FilePath, SaveMode))
    , optionsTimetable :: !(Maybe (FilePath, SaveMode))
    }
  deriving stock Show

parseOptions :: IO Options
parseOptions = do
    arguments <- (\arguments -> if null arguments then ["--help"] else arguments) <$> getArgs
    options   <- Options.handleParseResult
        $ flip (Options.execParserPure Options.defaultPrefs) arguments
        $ Options.info (options <**> Options.helper) about
    
    let Options { .. } = options
    when (areFilePathsForEqual [optionsWeeks, optionsBellRings, optionsTimetable]) do
        hPutStrLn stderr "Filepaths cannot be equal to each other."
        exitFailure
    
    pure options

areFilePathsForEqual :: [Maybe (FilePath, SaveMode)] -> Bool
areFilePathsForEqual = doesHaveDuplicates . map fst . catMaybes
  where
    doesHaveDuplicates xs = length xs /= length (nub xs)

-- Parsers.

options :: Options.Parser Options
options = Options
    <$> Options.optional ( weeks     <|> prettyWeeks     )
    <*> Options.optional ( bellRings <|> prettyBellRings )
    <*> Options.optional ( timetable <|> prettyTimetable )

about :: Options.InfoMod Options
about = mconcat
    [ Options.briefDesc
    , Options.progDesc
        "Downloads timetable from \"old.gsu.by/physfac\"\n\
        \and serializes it into JSON"
    ]

weeks :: Options.Parser (FilePath, SaveMode)
weeks = fmap (, Save) $ Options.strOption $ mconcat
    [ Options.short 'w'
    , Options.metavar "FILEPATH"
    , Options.help "Where to place weeks \"above and under the line\""
    ]

prettyWeeks :: Options.Parser (FilePath, SaveMode)
prettyWeeks = fmap (, SavePretty) $ Options.strOption $ mconcat
    [ Options.short 'W'
    , Options.metavar "FILEPATH"
    , Options.help "`-w` producing formatted output"
    ]

bellRings :: Options.Parser (FilePath, SaveMode)
bellRings = fmap (, Save) $ Options.strOption $ mconcat
    [ Options.short 'b'
    , Options.metavar "FILEPATH"
    , Options.help "Where to place bell rings table"
    ]

prettyBellRings :: Options.Parser (FilePath, SaveMode)
prettyBellRings = fmap (, SavePretty) $ Options.strOption $ mconcat
    [ Options.short 'B'
    , Options.metavar "FILEPATH"
    , Options.help "`-b` producing formatted output"
    ]

timetable :: Options.Parser (FilePath, SaveMode)
timetable = fmap (, Save) $ Options.strOption $ mconcat
    [ Options.short 't'
    , Options.metavar "FILEPATH"
    , Options.help "Where to place Excel timetable"
    ]

prettyTimetable :: Options.Parser (FilePath, SaveMode)
prettyTimetable = fmap (, SavePretty) $ Options.strOption $ mconcat
    [ Options.short 'T'
    , Options.metavar "FILEPATH"
    , Options.help "`-t` producing formatted output"
    ]
