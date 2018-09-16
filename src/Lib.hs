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

data Line = Line
    { file:: Text.Text
    , number:: Int
    , content:: Text.Text
    }
type Variable = (Text.Text, Text.Text)
type Slide = [Line]

{--data SlideWithParameters = SlideWithParameters
    { content:: Slide
    , layout:: Layout
    }
    --}


removeWhitespace :: Text.Text -> Text.Text
removeWhitespace t =
    Text.replace " " ""
        $ Text.replace "\t" ""
        $ Text.replace "\n" "" t



{--
    Splits a list of lines into multiple lists of lines
    corresponding to different slides
---}
splitSlides :: Text.Text -> [Text.Text] -> [Slide]
splitSlides fileName fileContent =
    let
        foldFn :: Line -> [[Line]] -> [[Line]]
        foldFn Line{content = "--"} acc = [[]] ++ acc
        foldFn line (current: rest) = ((current ++ [line]): rest)
    in
        foldr foldFn [[]]
            $ fmap (\(num, line) -> Line fileName num line)
            $ zip (iterate (+1) 1) fileContent




-- Re-merging the slides
slideToString :: Slide -> Text.Text
slideToString lines =
    Text.intercalate "\n" $ fmap content lines

mergeSlides :: [Slide] -> Text.Text
mergeSlides slides =
    Text.intercalate (Text.append "newslide\n" slideSeparator)
        $ fmap slideToString slides
