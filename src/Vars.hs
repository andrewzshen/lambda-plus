module Vars where

import qualified Data.Set as Set

type T = Set.Set String

empty :: T 
empty = Set.empty

singleton :: String -> T
singleton = Set.singleton

union :: T -> T -> T
union = Set.union

unions :: [T] -> T
unions = Set.unions

member :: String -> T -> Bool
member = Set.member

diff :: T -> T -> T
diff = Set.difference

delete :: String -> T -> T
delete = Set.delete

deleteAll :: [String] -> T -> T
deleteAll xs t = foldr Set.delete t xs

toList :: T -> [String]
toList = Set.toList

fromList :: [String] -> T
fromList = Set.fromList