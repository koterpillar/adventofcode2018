{-# LANGUAGE LambdaCase #-}

module Day02 where

import Conduit

import Data.List

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Maybe

import Data.Set (Set)
import qualified Data.Set as Set

import Utils

test :: [String]
test = ["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"]

letmap :: String -> Map Char Int
letmap [] = Map.empty
letmap (c:cs) =
  let rest = letmap cs
   in Map.insertWith (+) c 1 rest

p1 :: String -> Bool
p1 st = any (\(_, cnt) -> cnt == 2) $ Map.toList $ letmap st

p2 :: String -> Bool
p2 st = any (\(_, cnt) -> cnt == 3) $ Map.toList $ letmap st

solve :: [String] -> Int
solve input = count p1 input * count p2 input
  where
    count pred lst = length $ filter pred lst

commonCandidates :: [a] -> [[a]]
commonCandidates [] = []
commonCandidates (c:cs) = cs : map (c :) (commonCandidates cs)

mergePlus :: Ord k => Map k Int -> Map k Int -> Map k Int
mergePlus m1 m2 = foldr z m1 $ Map.toList m2
  where
    z (k, v) = Map.insertWith (+) k v

ccmaps :: [String] -> Map String Int
ccmaps [] = Map.empty
ccmaps (box:rest) =
  mergePlus (Map.fromList $ zip (commonCandidates box) (repeat 1)) (ccmaps rest)

solve2 :: [String] -> Maybe String
solve2 input =
  listToMaybe $ map fst $ filter (\(k, v) -> v == 2) $ Map.toList $ ccmaps input

test2 :: [String]
test2 = ["abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz"]
