{-# Language OverloadedStrings #-}

module Animation (extractAnimatedChunks, animateSlide) where

import qualified Data.List as List
import qualified Data.Text as Text

import Types
    ( Layout
    , Line(..)
    , lineSatisfies
    , mapLine
    , TextLine
    , Variable
    , Slide
    , SlideWithParameters
    )

import Error
    ( makeError
    , Error
    )



type AnimatedChunk = [TextLine]



animateSlide :: Slide -> Error [Slide]
animateSlide original =
    case extractAnimated original of
        Left err -> Left err
        Right Nothing -> Right [original]
        Right (Just (before, animated, rest)) ->
            Right $ doAnimation before animated rest



doAnimation :: [TextLine] -> [TextLine] -> [TextLine] -> [Slide]
doAnimation before animated rest =
    let
        animContent = buildTriangle $ extractAnimatedChunks animated
    in
        fmap (\animContent -> before ++ animContent ++ rest) animContent


extractAnimatedChunks :: [TextLine] -> [[TextLine]]
extractAnimatedChunks [] = []
extractAnimatedChunks lines =
    let
        (first, rest) = List.break (lineSatisfies (== ">>")) lines
    in
        -- drop 1 because break adds the element to the list
        [first] ++ (extractAnimatedChunks $ List.drop 1 $ rest)



buildTriangle :: [[a]] -> [[a]]
buildTriangle (head:rest) =
    List.scanl (++) head rest


{-
  Extracts the animated portion of a slide
-}
extractAnimated :: Slide -> Error (Maybe ([TextLine], [TextLine], [TextLine]))
extractAnimated slide =
    let
        (before, rest) = List.break (lineSatisfies (== "[[(animated)")) slide
        (animated, rest') = List.break (lineSatisfies (== "]]")) $ List.drop 1 rest
    in
        case (before, animated, rest') of
            (before, [], []) -> Right Nothing
            (before, (firstAnimated:_), []) ->
                Left
                    $ makeError firstAnimated "Unclosed [[(animatedSection) above line"
            (before, animated, rest) -> Right $ Just (before, animated, List.drop 1 rest)



data ExtractionState
    -- Not inside a section
    = NoSection
    -- Inside a section that started on line <0> with current frame <1>
    | InSection (Line ()) Int

type AnimatedSection = Line (Int, Text.Text)

extractAnimatedSections :: Slide -> Error [AnimatedSection]
extractAnimatedSections slide =
    let
        section line frame content = mapLine (\_ -> (frame, content)) line

        inner :: Slide -> ExtractionState -> [AnimatedSection] -> Error [AnimatedSection]
        inner [] NoSection partial = Right partial
        inner [] (InSection start _) _ =
            Left $ makeError start $
                "Found end of animation chunk, expected end of animation frame"
        inner (current:rest) state partial =
            case state of
                NoSection ->
                    case Text.breakOn ">-" $ lineContent current of
                        (stringOnly, "") ->
                            inner (rest) NoSection (partial ++ [section current 0 stringOnly])

    in
        inner slide NoSection []
