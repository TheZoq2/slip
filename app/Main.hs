#!/bin/runhaskell
{-# Language OverloadedStrings #-}

import Data.Foldable (for_)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO
import qualified Data.Maybe as Maybe
import qualified System.Environment as Environment
import qualified System.Process as Process
import qualified System.IO.Error as Error
import Data.List (concat, intersperse)

slideSeparator :: Text.Text
slideSeparator = "--"

type Template = [Text.Text]
type Variable = (Text.Text, Text.Text)


splitSlides :: [Text.Text] -> [[Text.Text]]
splitSlides file =
    let
        foldFn :: [[Text.Text]] -> Text.Text -> [[Text.Text]]
        foldFn [] line = [[line]]
        foldFn acc slideSeparator = acc ++ [[]]
        foldFn (current: rest) line = ((current ++ [line]): rest)
    in
        foldl foldFn [] file



mergeSlides :: [[Text.Text]] -> [Text.Text]
mergeSlides slides =
    concat (Text.append "\n") $ intersperse [slideSeparator] slides


main :: IO ()
main = do
    IO.putStrLn "Hello world"
