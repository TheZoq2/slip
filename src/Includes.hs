module Includes
    () where


import qualified Data.Text as Text
import qualified Data.Text.IO as IO

import Types


data MaybeInclude
    = Normal Text.Text
    | Include Text.Text


maybeProcessInclude :: Text.Text -> MaybeInclude
maybeProcessInclude line =
    case Text.stripPrefix ">>include " line of
        Just filename -> Include filename
        Nothing -> Normal line


processIncludes :: [TextLine] -> [Line MaybeInclude]
processIncludes lines =
    fmap (mapLine maybeProcessInclude) lines



includeIoAction :: Text.Text -> IO [TextLine]
includeIoAction filename = do
    content <- IO.readFile filename
    lines <- Text.lines content
    Line filename $ zipWith (Line fileName) [1..] lines


