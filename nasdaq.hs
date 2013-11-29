 {-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import qualified Data.Text.Internal as TI
import Network.HTTP.Conduit (simpleHttp)
import Network (withSocketsDo)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy  as BL
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, attributeIs, content, element, fromDocument, child, descendant, attribute, checkName, anyElement,
                        ($//), ($/), (&/), (&|), (&//), (>=>), ($|))
import qualified Text.Regex
import Text.Regex.Posix
import Text.Regex.Base.RegexLike
import qualified System.IO as SIO
import qualified Control.Exception as E

tostrict_l :: BL.ByteString -> B.ByteString
tostrict_l = B.concat . BL.toChunks

datadir_l = "nasdaq/"

main = sequence_ $ map readpage_l [1] 

readpage_l page = do
  let starturl = "http://community.nasdaq.com/articles-by-category.aspx?category=forex+and+currencies&page=" ++ (show page)
  doc <- withSocketsDo $ simpleHttp starturl
  let cursor = fromDocument $ parseLBS doc
      articleurls = (map T.strip $
        cursor $// element "div" >=> attributeIs "class" "article"
        >=> child
        >=> child
          &// attribute "href")
  sequence_ $ map readarticle_l articleurls

readarticle_l url = do
  doc <- withSocketsDo $ simpleHttp $ T.unpack url
  let cursor = fromDocument $ parseLBS doc
      txt = (map T.strip $
        cursor $// element "div"
          >=> attributeIs "id" "articleText"
            &// checkName (/="script")
              &/ content)
      headline = head $
        cursor $//
          attributeIs "itemprop" "headline"
            &/ content
  do
    let (_, articleid) = T.breakOnEnd "cm" url
    outh <- SIO.openFile (datadir_l ++ T.unpack articleid ++ ".txt") SIO.WriteMode
    SIO.hSetEncoding outh SIO.utf8
    SIO.hPutStr outh $ T.unpack $ T.intercalate " " (headline:txt)
    SIO.hClose outh



