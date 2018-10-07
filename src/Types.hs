module Types
    ( Layout
    , Line(..)
    , lineSatisfies
    , Variable
    , TextLine
    , Slide
    , SlideWithParameters
    ) where

import qualified Data.Text as Text

data Layout
    = Default
    | TwoColumns


data Line a = Line
    { lineFile:: Text.Text
    , lineNumber:: Int
    , lineContent:: a
    } deriving Show


lineSatisfies :: (a -> Bool) -> Line a -> Bool
lineSatisfies pred line = 
    pred $ lineContent line


type TextLine = Line Text.Text


type Variable = (Text.Text, Text.Text)
type Slide = [Line Text.Text]


data SlideWithParameters = SlideWithParameters
    { slideContent:: Slide
    , slideLayout:: Layout
    }



