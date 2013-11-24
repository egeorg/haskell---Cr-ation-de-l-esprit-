module StringMultiset ( StringMultiset
                       , find
                       , insert
                       , delete
                       , to_list
                       , empty_multiset ) where

import Prelude hiding (lookup)
import Data.Map (Map, lookup, empty, singleton, alter, assocs)
import Data.Maybe (isJust, fromJust, fromMaybe)

data StringMultiset = StringMultiset (Map Char StringMultiset) Int deriving Show

empty_multiset = StringMultiset empty 0

find :: String -> StringMultiset -> Int
find [] (StringMultiset children counter) = counter
find (x:xs) (StringMultiset children _) =
  let subtree = lookup x children
  in if (isJust subtree)
       then (find xs (fromJust subtree))
       else 0
  
insert :: String -> StringMultiset -> StringMultiset
insert [] (StringMultiset children counter) =
  StringMultiset children (counter + 1)
insert (x:xs) (StringMultiset children counter) =
  let new_children = alter subInsert x children
      subInsert = Just . (insert xs) . fromMaybe (StringMultiset empty 0)
  in StringMultiset new_children counter

delete :: String -> StringMultiset -> StringMultiset
delete []     (StringMultiset children counter)   =
  StringMultiset children (min 0 (counter - 1))
delete (x:xs) t@(StringMultiset children counter) =
  let new_children = alter subDelete x children
      subDelete Nothing = Just t
      subDelete (Just sub_t) = Just (delete xs sub_t)
  in (StringMultiset new_children counter)

to_list :: StringMultiset -> [String]
to_list t = to_list' t ""

to_list' :: StringMultiset -> String -> [String]
to_list' (StringMultiset children counter) current_string =
  let children_list = assocs children
      subtree_arr =
        (children_list >>=
          (\(c, subtree) ->
            to_list' subtree (current_string ++ [c])))
  in if (counter > 0)
       then current_string:subtree_arr
       else subtree_arr
