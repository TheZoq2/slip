{-# Language OverloadedStrings #-}

module Lib
    ( slideSeparator
    , splitSlides
    , slideToString
    , mergeSlides
    ) where

import Data.Foldable (for_)
import qualified Data.Text as Text
import qualified Data.Maybe as Maybe
import qualified System.Environment as Environment
import qualified System.Process as Process
import qualified System.IO.Error as Error
import Data.List (concat, intersperse)

slideSeparator :: Text.Text
slideSeparator = "--"

data Layout
    = Default
    | TwoColumns

type Template = [Text.Text]
type Variable = (Text.Text, Text.Text)
type Slide = [Text.Text]
data SlideWithParameters = SlideWithParameters
    { content:: Slide
    , layout:: Layout
    }


removeWhitespace :: Text.Text -> Text.Text
removeWhitespace t =
    Text.replace " " ""
        $ Text.replace "\t" ""
        $ Text.replace "\n" "" t



{--
    Splits a list of lines into multiple lists of lines
    corresponding to different slides
---}
splitSlides :: [Text.Text] -> [Slide]
splitSlides file =
    let
        foldFn :: Text.Text -> [[Text.Text]] -> [[Text.Text]]
        foldFn "--" acc = [[]] ++ acc
        foldFn line (current: rest) = ((current ++ [line]): rest)
    in
        foldr foldFn [[]] file


{--
--}
readParameters :: Slide -> SlideWithParameters
readParameters slide =
    let
        foldFn acc line =
            Text.splitOn "="
                $ removeWhitespace line
    in
    foldr 



-- Re-merging the slides
slideToString :: Slide -> Text.Text
slideToString lines =
    Text.intercalate "\n" lines

mergeSlides :: [Slide] -> Text.Text
mergeSlides slides =
    Text.intercalate (Text.append "newslide\n" slideSeparator)
        $ fmap slideToString slides
