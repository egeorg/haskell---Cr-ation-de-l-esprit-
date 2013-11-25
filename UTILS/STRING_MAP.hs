module STRING_MAP ( STRING_MAP
                   , find
                   , insert
                   , delete
                   , empty_map ) where

import Prelude hiding (lookup)
import Data.Map (Map, lookup, empty, singleton, alter)
import Data.Maybe (isJust, fromJust, fromMaybe)

empty_map a = STRING_MAP empty Nothing

data STRING_MAP a = STRING_MAP (Map Char (STRING_MAP a)) (Maybe a) deriving Show

find :: String -> (STRING_MAP a) -> (Maybe a)
find [] (STRING_MAP children payload) = payload
find (x:xs) (STRING_MAP children _) =
  let subtree = lookup x children
  in if (isJust subtree) 
    then (find xs (fromJust subtree))
    else Nothing
  
insert :: String -> a -> (STRING_MAP a) -> (STRING_MAP a)  
insert [] new_payload (STRING_MAP children _) =
  STRING_MAP children (Just new_payload)
insert (x:xs) new_payload (STRING_MAP children payload) =
  let new_children = alter subInsert x children
      subInsert = Just . (insert xs new_payload) . fromMaybe (STRING_MAP empty Nothing)
  in  STRING_MAP new_children payload
                                                    
delete :: String -> (STRING_MAP a) -> (STRING_MAP a)
delete [] (STRING_MAP children _) = STRING_MAP children Nothing
delete (x:xs) t@(STRING_MAP children payload) =
  let new_children = alter subDelete x children
      subDelete Nothing = Just t
      subDelete (Just sub_t) = Just (delete xs sub_t)
  in  (STRING_MAP new_children payload)
