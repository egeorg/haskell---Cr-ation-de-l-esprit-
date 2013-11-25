module STRING_MULTISET ( STRING_MULTISET
                       , find
                       , insert
                       , delete
                       , to_list
                       , empty_multiset ) where

import Prelude hiding (lookup)
import Data.Map (Map, lookup, empty, singleton, alter, assocs)
import Data.Maybe (isJust, fromJust, fromMaybe)

data STRING_MULTISET = STRING_MULTISET (Map Char STRING_MULTISET) Int deriving Show

empty_multiset = STRING_MULTISET empty 0

find :: String -> STRING_MULTISET -> Int
find [] (STRING_MULTISET children counter) = counter
find (x:xs) (STRING_MULTISET children _) =
  let subtree = lookup x children
  in if (isJust subtree)
       then (find xs (fromJust subtree))
       else 0
  
insert :: String -> STRING_MULTISET -> STRING_MULTISET
insert [] (STRING_MULTISET children counter) =
  STRING_MULTISET children (counter + 1)
insert (x:xs) (STRING_MULTISET children counter) =
  let new_children = alter subInsert x children
      subInsert = Just . (insert xs) . fromMaybe (STRING_MULTISET empty 0)
  in STRING_MULTISET new_children counter

delete :: String -> STRING_MULTISET -> STRING_MULTISET
delete []     (STRING_MULTISET children counter)   =
  STRING_MULTISET children (min 0 (counter - 1))
delete (x:xs) t@(STRING_MULTISET children counter) =
  let new_children = alter subDelete x children
      subDelete Nothing = Just t
      subDelete (Just sub_t) = Just (delete xs sub_t)
  in (STRING_MULTISET new_children counter)

to_list :: STRING_MULTISET -> [String]
to_list t = to_list' t ""

to_list' :: STRING_MULTISET -> String -> [String]
to_list' (STRING_MULTISET children counter) current_string =
  let children_list = assocs children
      subtree_arr =
        (children_list >>=
          (\(c, subtree) ->
            to_list' subtree (current_string ++ [c])))
  in if (counter > 0)
       then current_string:subtree_arr
       else subtree_arr
