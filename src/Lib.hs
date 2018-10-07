{-# Language OverloadedStrings #-}

module Lib
    ( slideSeparator
    , splitSlides
    , slideToString
    , mergeSlides
    , unsafeAnimateSlides
    ) where

import Data.Foldable (for_)
import qualified Data.Text as Text
import qualified Data.Maybe as Maybe
import qualified System.Environment as Environment
import qualified System.Process as Process
import qualified System.IO.Error as Error
import qualified Data.Either as Either
import Data.List (concat, intersperse)
import Types
    ( Layout
    , Line(..)
    , Variable
    , Slide
    , SlideWithParameters
    )
import Animation

slideSeparator :: Text.Text
slideSeparator = "--"





{-
  Applies a transformation to the specified line without mangling line numbers
-}
transformLine :: (a -> b) -> Line a -> Line b
transformLine fn line =
    Line (lineFile line) (lineNumber line) $ fn $ lineContent line







removeWhitespace :: Text.Text -> Text.Text
removeWhitespace t =
    Text.replace " " ""
        $ Text.replace "\t" ""
        $ Text.replace "\n" "" t



{--
    Splits a list of lines into multiple lists of lines
    corresponding to different slides
--}
splitSlides :: Text.Text -> [Text.Text] -> [Slide]
splitSlides fileName fileContent =
    let
        foldFn :: Line Text.Text -> [Slide] ->  [Slide]
        foldFn Line{lineContent = "--"} acc = []: acc
        foldFn line (current: rest) = ((line: current): rest)
    in
        foldr foldFn [[]]
            $ zipWith (Line fileName) [1..] fileContent



unsafeAnimateSlides :: [Slide] -> [Slide]
unsafeAnimateSlides slides =
    let
        animated = fmap (\slide -> Either.fromRight [slide] $ animateSlide slide) slides
    in
        concat animated


-- Re-merging the slides
slideToString :: Slide -> Text.Text
slideToString lines =
    Text.intercalate "\n" $ fmap lineContent lines




mergeSlides :: [Slide] -> Text.Text
mergeSlides slides =
    Text.intercalate (Text.append "\n" slideSeparator)
        $ fmap slideToString slides
