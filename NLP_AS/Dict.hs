module Dict ( Dict
            , find
            , insert
            , delete
            , to_list
            , empty_dict ) where

import Prelude hiding (lookup)
import Data.Map (Map, lookup, empty, singleton, alter, assocs)
import Data.Maybe (isJust, fromJust, fromMaybe)

data Dict = Dict (Map Char Dict) Bool deriving Show

empty_dict = Dict empty False

find :: String -> Dict -> Bool
find [] (Dict children isLeaf) = isLeaf
find (x:xs) (Dict children _) =
  let subtree = lookup x children
  in (isJust subtree) && (find xs (fromJust subtree))
  
insert :: String -> Dict -> Dict
insert [] (Dict children _) = Dict children True
insert (x:xs) (Dict children isLeaf) =
  let new_children = alter subInsert x children
      subInsert = Just . (insert xs) . fromMaybe (Dict empty False)
  in Dict new_children isLeaf

delete :: String -> Dict -> Dict
delete [] (Dict children _) = Dict children False
delete (x:xs) t@(Dict children isLeaf) =
  let new_children = alter subDelete x children
      subDelete Nothing = Just t
      subDelete (Just sub_t) = Just (delete xs sub_t)
  in (Dict new_children isLeaf)

to_list :: Dict -> [String]
to_list t = to_list' t ""

to_list' :: Dict -> String -> [String]
to_list' (Dict children isLeaf) current_string =
  let children_list = assocs children
      subtree_arr =
        (children_list >>= 
          (\(c, subtree) -> 
            to_list' subtree (current_string ++ [c])))
  in if isLeaf
    then current_string:subtree_arr
    else subtree_arr
