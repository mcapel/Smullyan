module Aux where
import Data
import Data.List

type Assignment = [(Int,[Char])]

toSet :: Ord a => [a] -> [a]
toSet = sort.nub

updateAssignments :: (Int,[Char]) -> [Assignment] -> [Assignment]
updateAssignments _ [] = []
updateAssignments a (as:ass) = 
   case update a as of
      [] -> updateAssignments a ass
      as' -> as': updateAssignments a ass

update :: (Int,[Char]) -> Assignment -> Assignment 
update = update' []
  where
   update' :: Assignment -> (Int,[Char]) -> Assignment -> Assignment
   update' ts (i,vs) [] = (i,vs):ts
   update' ts (i,vs) ((k,vs'):tss) 
         | i/=k = update' ((k,vs'):ts) (i,vs) tss
         | i==k && consistents vs vs' = toSet ((k,toSet (vs++vs')):ts++tss)
         | otherwise = []

consistentMapUnion :: Assignment -> Assignment -> Maybe Assignment
consistentMapUnion [] a2 = Just a2
consistentMapUnion (a:as) a2 = 
   case update a a2 of
      [] -> Nothing
      a2' -> consistentMapUnion as a2'

consistents :: [Char] -> [Char] -> Bool
consistents [] _ = True
consistents (y:ys) xs = consistent y xs && consistents ys xs

consistent :: Char -> [Char] -> Bool
consistent x xs = not (elem x' xs)
   where x' = dual x

consistentSet :: [Char] -> Bool
consistentSet [] = True
consistentSet (a:as) = 
   consistent a as && consistentSet as

pruneSubsumed :: [Assignment] -> [Assignment]
pruneSubsumed as = pruneSubsumed' [] as

pruneSubsumed' :: [Assignment] -> [Assignment] -> [Assignment]
pruneSubsumed' bs [] = bs
pruneSubsumed' bs ([]:as) = pruneSubsumed' bs as 
pruneSubsumed' bs (a:as) 
   | or [ subsetMap a' a | a' <- bs++as ] = pruneSubsumed' bs as
   | otherwise = pruneSubsumed' (a:bs) as

subsetMap [] _ = True
subsetMap (a:as) bs = elemMap a bs && subsetMap as bs

elemMap _ [] = False
elemMap (i,vs) ((k,vs'):bs) =
    (i==k && all (\ x -> elem x vs') vs) || elemMap (i,vs) bs

