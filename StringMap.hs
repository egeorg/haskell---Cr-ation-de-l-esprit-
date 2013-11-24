module StringMap ( StringMap
                 , find
                 , insert
                 , delete
                 , empty_map ) where

import Prelude hiding (lookup)
import Data.Map (Map, lookup, empty, singleton, alter)
import Data.Maybe (isJust, fromJust, fromMaybe)

empty_map a = StringMap empty Nothing

data StringMap a = StringMap (Map Char (StringMap a)) (Maybe a) deriving Show

find :: String -> (StringMap a) -> (Maybe a)
find [] (StringMap children payload) = payload
find (x:xs) (StringMap children _) =
  let subtree = lookup x children
  in if (isJust subtree) 
    then (find xs (fromJust subtree))
    else Nothing
  
insert :: String -> a -> (StringMap a) -> (StringMap a)  
insert [] new_payload (StringMap children _) =
  StringMap children (Just new_payload)
insert (x:xs) new_payload (StringMap children payload) =
  let new_children = alter subInsert x children
      subInsert = Just . (insert xs new_payload) . fromMaybe (StringMap empty Nothing)
  in  StringMap new_children payload
                                                    
delete :: String -> (StringMap a) -> (StringMap a)
delete [] (StringMap children _) = StringMap children Nothing
delete (x:xs) t@(StringMap children payload) =
  let new_children = alter subDelete x children
      subDelete Nothing = Just t
      subDelete (Just sub_t) = Just (delete xs sub_t)
  in  (StringMap new_children payload)
