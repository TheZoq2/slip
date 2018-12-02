{-# Language OverloadedStrings #-}

import Lib
    ( slideSeparator
    , splitSlides
    , slideToString
    , mergeSlides
    , animateSlides
    )

import qualified Data.Text as Text
import qualified Data.Text.IO as IO




main :: IO ()
main = do
    content <- IO.readFile "slip_input.md"
    let contentLines = Text.lines content
    IO.putStrLn $ either id mergeSlides $ animateSlides $ splitSlides "content_example.md" contentLines
