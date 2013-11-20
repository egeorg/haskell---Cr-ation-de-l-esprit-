{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import Network.HTTP.Conduit (simpleHttp)
import Network (withSocketsDo)
import qualified Data.Text as T
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, attributeIs, content, element, fromDocument, child, descendant,
                        ($//), (&|), (&//), (>=>))
 
main :: IO ()
main = do
  doc <- withSocketsDo $ simpleHttp "http://www.nasdaq.com/article/forex-flash-the-australian-dollar-reached-almost-106-before-the-unwinding-of-cross-positions-against-the-yen-took-a-toll-bbh-cm236594"
  let cursor = fromDocument $ parseLBS doc
  print $ T.concat $
    child cursor >>= element "head" >>= child
                 >>= element "title" >>= descendant
                 >>= content
