{-# Language OverloadedStrings #-}
module Error
    ( makeError
    , Error
    ) where

import qualified Data.Text as Text

import Types (Line(..))


type Error a = Either Text.Text a


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



