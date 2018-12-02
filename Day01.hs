{-# LANGUAGE LambdaCase #-}
module Day01 where

import Conduit

import Data.List

import Data.Set (Set)
import qualified Data.Set as Set

import Utils

getInput :: IO [Int]
getInput = map (read . filter (/= '+')) <$> readLines

test1 :: [Int]
test1 = [3, 3, 4, -2, -4]

firstSumTwice :: Monad m => ConduitM Int Void m (Maybe Int)
firstSumTwice = go (Set.singleton 0) 0
  where
    go seen current =
      await >>= \case
        Nothing -> pure Nothing
        Just num ->
          let next = current + num
           in if Set.member next seen
                then pure (Just next)
                else go (Set.insert next seen) next
