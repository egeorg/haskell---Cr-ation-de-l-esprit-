module STRING_SET ( STRING_SET
                  , find
                  , insert
                  , delete
                  , to_list
                  , empty_set ) where

import Prelude hiding (lookup)
import Data.Map (Map, lookup, empty, singleton, alter, assocs)
import Data.Maybe (isJust, fromJust, fromMaybe)

data STRING_SET = STRING_SET (Map Char STRING_SET) Bool deriving Show

empty_set = STRING_SET empty False

find :: String -> STRING_SET -> Bool
find [] (STRING_SET children isLeaf) = isLeaf
find (x:xs) (STRING_SET children _) =
  let subtree = lookup x children
  in (isJust subtree) && (find xs (fromJust subtree))
  
insert :: String -> STRING_SET -> STRING_SET
insert [] (STRING_SET children _) = STRING_SET children True
insert (x:xs) (STRING_SET children isLeaf) =
  let new_children = alter subInsert x children
      subInsert = Just . (insert xs) . fromMaybe (STRING_SET empty False)
  in STRING_SET new_children isLeaf

delete :: String -> STRING_SET -> STRING_SET
delete [] (STRING_SET children _) = STRING_SET children False
delete (x:xs) t@(STRING_SET children isLeaf) =
  let new_children = alter subDelete x children
      subDelete Nothing = Just t
      subDelete (Just sub_t) = Just (delete xs sub_t)
  in (STRING_SET new_children isLeaf)

to_list :: STRING_SET -> [String]
to_list t = to_list' t ""

to_list' :: STRING_SET -> String -> [String]
to_list' (STRING_SET children isLeaf) current_string =
  let children_list = assocs children
      subtree_arr =
        (children_list >>= 
          (\(c, subtree) -> 
            to_list' subtree (current_string ++ [c])))
  in if isLeaf
    then current_string:subtree_arr
    else subtree_arr
