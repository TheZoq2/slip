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

data Line a = Line
    { lineFile:: Text.Text
    , lineNumber:: Int
    , lineContent:: a
    }
type Variable = (Text.Text, Text.Text)
type Slide = [Line Text.Text]


data SlideWithParameters = SlideWithParameters
    { slideContent:: Slide
    , slideLayout:: Layout
    }


type Error a = Eiter a Text.Teat



makeError :: Line a -> Text.Text -> Text.Text
makeError line err =
    Text.concat
        [ "Error at "
        , lineFile line
        , ":"
        , Text.pack $ show $ lineNumber line
        , "\n\t"
        , err
        ]


{-
  Applies a transformation to the specified line without mangling line numbers
-}
transformLine :: (a -> b) -> Line a -> Line b
transformLine fn line =
    Line (lineFile line) (lineNumber line) $ fn $ lineContent line




getVariable :: Line Data.Text -> Error (Either (Line Text.Text) (Line Variable))
getVariable line =
    let
        parseVariable line =

    in
    case Text.take 1 line of
        "$" ->
            parseVariable $ drop 1 line
        _ -> Left line






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
        foldFn :: Line Text.Text -> [[Line Text.Text]] -> [[Line Text.Text]]
        foldFn Line{lineContent = "--"} acc = [[]] ++ acc
        foldFn line (current: rest) = ((current ++ [line]): rest)
    in
        foldr foldFn [[]]
            $ fmap (\(num, line) -> Line fileName num line)
            $ zip (iterate (+1) 1) fileContent




-- Re-merging the slides
slideToString :: Slide -> Text.Text
slideToString lines =
    Text.intercalate "\n" $ fmap lineContent lines

mergeSlides :: [Slide] -> Text.Text
mergeSlides slides =
    Text.intercalate (Text.append "newslide\n" slideSeparator)
        $ fmap slideToString slides
