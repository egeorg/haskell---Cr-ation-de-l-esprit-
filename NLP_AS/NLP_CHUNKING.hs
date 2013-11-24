module NLP_CHUNKING ( readchunks_str
                    , readchunks_file) where

import qualified Text.Regex
--import qualified Data.ByteString as S
--import qualified Data.ByteString.Char8 as C
import Text.Regex.Posix
import Text.Regex.Base.RegexLike

--TODO: my be very inefficient
readchunks_str :: String -> [Char] -> [String]
readchunks_str str delimiters =
  let regex = "[^" ++ delimiters ++ "]+"
  in (getAllTextMatches (str =~ regex) :: [String])
    
readchunks_file :: String -> String -> IO [String]
readchunks_file source_path delimiters_path =
    do source_str <- (readFile source_path)
       delimiters_str <- (readFile delimiters_path)
       let regex = "[^" ++ delimiters_str ++ "]+"
       return (getAllTextMatches (source_str =~ regex) :: [String])