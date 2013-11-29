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
                        ($//), ($/), (&/), (&|), (&//), (>=>))
import qualified Text.Regex
import Text.Regex.Posix
import Text.Regex.Base.RegexLike
import qualified System.IO as SIO
import qualified Control.Exception as E

datadir_l = "forex-mmcis/"

tostrict_l :: BL.ByteString -> B.ByteString
tostrict_l = B.concat . BL.toChunks

isleap_l = (==2012)

days_l year month =
  let days28 = [1..28]
  in
    if (month == 2)
      then
        if (isleap_l year)
          then days28 ++ [29]
          else days28
      else
        if (elem month [1, 3, 5, 7, 8, 10, 12])
          then days28 ++ [29, 30, 31]
          else days28 ++ [29, 30]

main :: IO ()
main = 
  let months = [1..12]
      years  = [2011..2013]
  in
    do outh <- SIO.openFile (datadir_l ++ "output.txt") SIO.WriteMode
       SIO.hSetEncoding outh SIO.utf8
       sequence_ $
         years >>=
           (\y -> 
             months >>= 
               (\m -> 
                 (days_l y m) >>= 
                   (\d -> [getnewsbydate_l outh y m d])))
       SIO.hClose outh

stubbornrequest_l url =
  E.handle errorHandler
           (simpleHttp url)
    where errorHandler :: E.SomeException -> (IO BL.ByteString)
          errorHandler _ = stubbornrequest_l url
         
getnewsbydate_l outh year month day = do
  let starturl = "http://forex-mmcis.com/dj_forex.html?d=" ++ (show day) ++ "&m=" ++ (show month) ++ "&y=" ++ (show year) ++ "&to=back"
  doc_wrong <- withSocketsDo $ stubbornrequest_l starturl
  let doc = BL.fromChunks [B.pack . T.unpack $ T.replace "</div</td>" "</div></td>" $ T.pack . B.unpack $ tostrict_l doc_wrong]
      cursor = fromDocument $ parseLBS doc
      newsbody = (map T.strip $
        cursor $// element "td" >=> attributeIs "class" "t_padding"
        >=> child
        >=> checkName (=="p")
                &/ content)
      newstime = (map T.strip $
        cursor $// attributeIs "class" "time"
                &/ content)
  SIO.hPutStr outh $ T.unpack . T.concat $ zipWith (format_l (T.pack $ show year) (T.pack $ show month) (T.pack $ show day)) newstime newsbody

format_l year month day time body = T.concat [T.intercalate "/" [year, month, day, time], ", ", body, "\n"]

                
                