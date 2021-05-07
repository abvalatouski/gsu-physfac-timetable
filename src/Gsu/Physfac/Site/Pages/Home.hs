module Gsu.Physfac.Site.Pages.Home
  where

import           GHC.Generics
import           Prelude                 hiding (div, span)

import           Data.Aeson              (ToJSON)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.Read          as Text
import qualified Text.XML.Stream.Parse   as Xml

import           Gsu.Physfac.Site.Parser

data HomePage = HomePage
    { homePageBellRings :: BellRings
    , homePageWeekPairs :: [WeekPair]
    }
  deriving stock Show

{-# ANN homePage ("HLint: ignore" :: String) #-}
homePage :: Parser HomePage
homePage =
    html do
        head_
        body do
            inner <- div do -- #center
                inner <- div do -- #body_outline
                    div_ -- #con_u_32
                    div_ -- .clr
                    div_ -- #header_area
                    div_ -- #user_3_outline
                    div_ -- .clr
                    div_ -- #con_left
                    inner <- div do -- #sandbox_1
                        inner <- div do -- #con_right
                            inner <- div do -- #right_outline
                                inner <- div do -- .moduletable
                                    h3_
                                    table_ -- .contentpaneopen
                                    span_ -- .article_separator
                                    homePageBellRings <- bellRings
                                    span_ -- .article_separator
                                    homePageWeekPairs <- weekPairs
                                    span_ -- .article_separator
                                    table_ -- .contentpaneopen
                                    span_ -- .article_separator
                                    table_ -- .contentpaneopen
                                    span_ -- .article_separator
                                    pure HomePage { .. }
                                div_ -- .moduletable
                                pure inner
                            div_ -- #user_4_outline
                            pure inner
                        div_ -- #con_main1
                        pure inner
                    div_ -- .cls
                    div_ -- #footer
                    div_ -- .clr
                    pure inner
                insertMissingEndTag "div"
                pure inner
            div_
            div_ -- #lbOverlay
            div_ -- #lbCenter
            div_ -- #lbBottomContainer
            pure inner

-- Page components.

type BellRings = [BellRingQuad]

bellRings :: Parser BellRings
bellRings =
    table do
        inner <- tr do
            td do
                inner <- div do
                    table do
                        tbody do
                            tr_
                            Xml.many $ tr' bellRingQuad
                hr_
                pure inner
        tr_
        pure inner

data BellRingQuad = BellRingQuad
    { half1 :: !BellRingPair
    , half2 :: !BellRingPair
    }
  deriving stock    (Show, Generic)
  deriving anyclass ToJSON

bellRingQuad :: Parser BellRingQuad
bellRingQuad = BellRingQuad <$> bellRingPair <*> bellRingPair

data BellRingPair = BellRingPair
    { begin :: !HoursAndMinutes
    , end   :: !HoursAndMinutes
    }
  deriving stock    (Show, Generic)
  deriving anyclass ToJSON

data HoursAndMinutes = HoursAndMinutes
    { hours   :: !Int
    , minutes :: !Int
    }
  deriving stock    (Show, Generic)
  deriving anyclass ToJSON

bellRingPair :: Parser BellRingPair
bellRingPair = td $ div $ span $ span do
    text <- Xml.content
    let [begin, end] = map (toTime . Text.strip) $ Text.splitOn "-" text
    pure $ BellRingPair begin end
  where
    toTime input =
        let [hours, minutes] = map toInt $ Text.splitOn "." input
         in HoursAndMinutes hours minutes

data WeekPair = WeekPair
    { aboveTheLine :: !MonthAndDayPair
    , underTheLine :: !MonthAndDayPair
    }
  deriving stock    (Show, Generic)
  deriving anyclass ToJSON

data MonthAndDay = MonthAndDay
    { month :: !Int
    , day   :: !Int
    }
  deriving stock    (Show, Generic)
  deriving anyclass ToJSON

data MonthAndDayPair = MonthAndDayPair
    { from :: !MonthAndDay
    , to   :: !MonthAndDay
    }
  deriving stock    (Show, Generic)
  deriving anyclass ToJSON

weekPairs :: Parser [WeekPair]
weekPairs =
    table do
        inner <- tr do
            td do
                inner <- div do
                    table do
                        tbody do
                            tr_
                            tr_
                            tr_
                            Xml.many $ tr' weekPair
                hr_
                pure inner
        tr_
        pure inner

weekPair :: Parser WeekPair
weekPair = WeekPair <$> week <*> week

week :: Parser MonthAndDayPair
week = td $ div $ span $ span $ toPair <$> Xml.content
  where
    toPair input =
        let [from, to] = map Text.strip $ Text.splitOn "-" input
         in MonthAndDayPair (toDate from) (toDate to)
    
    toDate input =
        let [day, month] = map toInt $ Text.splitOn "." input
         in MonthAndDay month day

-- Utils.

toInt :: Text -> Int
toInt input =
    let Right (result, "") = Text.decimal input
     in result
