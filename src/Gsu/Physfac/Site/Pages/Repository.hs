module Gsu.Physfac.Site.Pages.Repository
  where

import           Prelude                 hiding (div)

import           Data.Text               (Text)

import           Gsu.Physfac.Common
import           Gsu.Physfac.Site.Parser

type Repository = [RepositoryFile]

{-# ANN repository ("HLint: ignore" :: String) #-}
repository :: Parser Repository
repository =
    html do
        head_
        body do
            div do
                inner <- div do -- #body_outline
                    div_ -- #con_u_32
                    div_ -- .clr
                    div_ -- #header_area
                    div_ -- #user_3_outline
                    div_ -- .clr
                    div_ -- #con_left
                    inner <- div do -- #sandbox_1
                        div_ -- #con_right
                        div do -- #con_main1
                            div_ -- #pathway_out_1
                            div_ -- .clr
                            div do -- #main_out_1
                                div do -- #remository
                                    div_ -- #remositorypathway
                                    div_ -- #remositorypageheading
                                    div_ -- #remositorycontainer
                                    div_ -- #remositoryfilelisthead
                                    inner <- div do -- #remositoryfilelisting
                                        many $ div' -- #remositoryfileblock
                                            repositoryFile
                                    script_
                                    div_ -- #remositoryfooter
                                    div_ -- #remositorycredits
                                    pure inner
                    div_ -- .clr
                    div_ -- #footer
                    div_ -- .clr
                    pure inner
                insertMissingEndTag "div"
                pure inner

data RepositoryFile = RepositoryFile
    { repositoryFileUrl   :: !Text
    , repositoryFileImage :: !Text
    , repositoryFileName  :: !Text
    }
  deriving stock Show

repositoryFile :: Parser RepositoryFile
repositoryFile = do
    inner <- h3 do
        a \href -> RepositoryFile href <$> img pure <*> content
    div_ -- #remositoryonethumb
    div_ -- #remositoryfilesummary
    pure inner
