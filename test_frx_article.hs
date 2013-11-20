{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import Network.HTTP.Conduit (simpleHttp)
import Network (withSocketsDo)
import qualified Data.Text as T
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, attributeIs, content, element, fromDocument, child, descendant,
                        ($//), ($/), (&/), (&|), (&//), (>=>))
starturl = "http://www.nasdaq.com/article/forex-flash-the-australian-dollar-reached-almost-106-before-the-unwinding-of-cross-positions-against-the-yen-took-a-toll-bbh-cm236594"
						
main :: IO ()
main = do
  doc <- withSocketsDo $ simpleHttp starturl
  let cursor = fromDocument $ parseLBS doc
  print $ T.concat $
	cursor $// element "div"
	>=> attributeIs "id" "articleText"
	>=> child
	>=> element "p"
	&// content
