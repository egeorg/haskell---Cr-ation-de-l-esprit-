{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import Network.HTTP.Conduit (simpleHttp)
import Network (withSocketsDo)
import qualified Data.Text as T
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, attributeIs, content, element, fromDocument, child, descendant, attribute,
                        ($//), ($/), (&/), (&|), (&//), (>=>))
starturl = "http://community.nasdaq.com/articles-by-category.aspx?category=forex+and+currencies&page=838"
						
main :: IO ()
main = do
  doc <- withSocketsDo $ simpleHttp starturl
  let cursor = fromDocument $ parseLBS doc
  print $ T.concat $
	cursor $// element "p"
	>=> attributeIs "class" "la_articleinfo"
	&// content
