{-# Language OverloadedStrings #-}

import Lib
    ( slideSeparator
    , splitSlides
    , slideToString
    , mergeSlides
    )

import qualified Data.Text as Text
import qualified Data.Text.IO as IO


main :: IO ()
main = do
    content <- IO.readFile "content_example.md"
    let contentLines = Text.lines content
    IO.putStrLn $ mergeSlides $ splitSlides contentLines
