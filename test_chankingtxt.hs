module Parser ( read_chunks ) where

import qualified Data.Text as T
import qualified Text.Regex as T
--import qualified Data.ByteString as S
--import qualified Data.ByteString.Char8 as C
import Text.Regex.Posix
import Text.Regex.Base.RegexLike
import Prelude hiding (readFile, appendFile, writeFile)
import System.IO.UTF8 (readFile, appendFile, writeFile)


read_chunks :: String -> String -> IO [String]
read_chunks source_path delimiters_path =
    do source_str <- (readFile source_path)
       delimiters_str <- (readFile delimiters_path)
       let t = "[^" ++ delimiters_str ++ "]+"
       return (getAllTextMatches (source_str =~ t) :: [String])

read_chunks_to_file :: String -> String -> String -> IO ()
read_chunks_to_file source_path delimiters_path out_path =
    do source_str <- (readFile source_path)
       delimiters_str <- (readFile delimiters_path)
       let t = "[^" ++ delimiters_str ++ "]+"
           chunks = getAllTextMatches (source_str =~ t) :: [String]
       writeFile out_path $ head chunks
       sequence_ $
         map (\str -> (appendFile out_path $ '\n':str))
             (tail chunks)

