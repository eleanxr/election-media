{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, attributeIs, content, element,
    fromDocument, child, ($//), (&|), (&//), (>=>))
import NLP.POS (defaultTagger, brownTagger, conllTagger, tagText, tag)
import NLP.Types (POSTagger)
import NLP.Types.Tree (TaggedSentence)
import NLP.Types.Tags (Tag)
import Control.Monad
import HeadlineDB

url = "http://www.nytimes.com"

-- Selector for headline nodes.
findNodes :: Cursor -> [Cursor]
findNodes = attributeIs "class" "story-heading" >=> ($// child)

-- Extract text content from selected nodes.
extractData = T.concat . content

-- Remove leading and trailing whitespace and newlines and remove
-- empty values.
cleanContent :: [T.Text] -> [T.Text]
cleanContent = filter (not . T.null) . (fmap $ T.strip)

tagHeadlines :: Tag t => IO (POSTagger t) -> [T.Text] -> IO [[TaggedSentence t]]
tagHeadlines wrappedTagger lines =
    wrappedTagger >>= \tagger ->
    return $ (tag tagger) `fmap` lines

-- Handle selected data from page.
processData :: [T.Text] -> IO ()
processData texts =
    tagHeadlines brownTagger (cleanContent texts) >>= \taggedLines ->
    mapM_ putStrLn $ map show taggedLines

-- Get a Cursor for the specified URL.
cursorFor :: String -> IO Cursor
cursorFor u = do
    page <- simpleHttp u
    return $ fromDocument $ parseLBS page

-- Print the content in the selected nodes.
main = do
    cursor <- cursorFor url
    processData $ cursor $// findNodes &| extractData
