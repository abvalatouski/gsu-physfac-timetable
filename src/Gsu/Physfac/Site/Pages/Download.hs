module Gsu.Physfac.Site.Pages.Download
  where

import           Prelude                 hiding (div)

import           Data.Text               (Text)

import           Gsu.Physfac.Common
import           Gsu.Physfac.Site.Parser

type DownloadLink = Text

{-# ANN downloadLink ("HLint: ignore" :: String) #-}
downloadLink :: Parser DownloadLink
downloadLink =
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
                    div_ -- .con_left
                    inner <- div do -- #sandbox_1
                        div_ -- #con_right
                        div do -- #con_main1
                            div_ -- #pathway_out_1
                            div_ -- .clr
                            div do -- #main_out_1
                                div do -- #remository
                                    div_ -- #remositorypathway
                                    inner <- div do -- #remositoryfileinfo
                                        script_
                                        inner <- h2 do
                                            content_
                                            a \href -> do
                                                img_
                                                strong_
                                                pure href
                                        dl_
                                        pure inner
                                    div_ -- #remositoryfooter
                                    pure inner
                    div_ -- .clr
                    div_ -- #footer
                    div_ -- .clr
                    pure inner
                insertMissingEndTag "div"
                pure inner
