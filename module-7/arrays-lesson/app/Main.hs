{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Array.Base (STUArray (STUArray), UArray (UArray))
import Data.Array.ST (STUArray, freeze, newArray, readArray, runSTUArray, thaw, writeArray)
import Data.Array.Unboxed (UArray, accum, array, bounds, listArray, (!), (//))

zeroIndexArray :: UArray Int Bool
zeroIndexArray = array (0, 9) [(3, True)]

qcArray :: UArray Int Bool
qcArray = array (0, 4) [(1, True), (2, True)]

beansInBuckets :: UArray Int Int
beansInBuckets = array (0, 3) []

updatedBiB :: UArray Int Int
updatedBiB = beansInBuckets // [(1, 5), (3, 6)]

doubled :: UArray Int Int
doubled = accum (+) updatedBiB $ map (,2) [0 .. 3]

listToUArray :: [Int] -> UArray Int Int
listToUArray vals = runSTUArray $ do
  let end = length vals - 1
  stArray <- newArray (0, end) 0
  forM_ [0 .. end] $ \i -> do
    let val = vals !! i
    writeArray stArray i val
  return stArray

myData :: UArray Int Int
myData = listArray (0, 5) [7, 6, 4, 8, 10, 2]

bubbleSort :: UArray Int Int -> UArray Int Int
bubbleSort myArray = runSTUArray $ do
  stArray <- thaw myArray
  let end = (snd . bounds) myArray
  forM_ [1 .. end] $ \i -> do
    forM_ [0 .. (end - i)] $ \j -> do
      val <- readArray stArray j
      nextVal <- readArray stArray (j + 1)
      let outOfOrder = val > nextVal
      when outOfOrder $ do
        writeArray stArray j nextVal
        writeArray stArray (j + 1) val
  return stArray

c1 :: UArray Int Int
c1 = array (0, 4) $ map (,1) [0 .. 4]

c2 :: UArray Int Int
c2 = array (0, 4) $ map (,0) [0 .. 4]

cross :: (UArray Int Int, UArray Int Int) -> Int -> (UArray Int Int, UArray Int Int)
cross (a1, a2) _index = runST $ do
  thaw1 <- thaw a1 :: ST s (STUArray s Int Int)
  thaw2 <- thaw a2 :: ST s (STUArray s Int Int)
  let end = (snd . bounds) a1
  forM_ [_index .. end] $ \i -> do
    el1 <- readArray thaw1 i
    el2 <- readArray thaw2 i
    writeArray thaw1 i el2
    writeArray thaw2 i el1
  freeze1 <- freeze thaw1
  freeze2 <- freeze thaw2
  return (freeze1, freeze2)

crossRes :: (UArray Int Int, UArray Int Int)
crossRes = cross (c1, c2) 3

replaceZeros :: UArray Int Int -> UArray Int Int
replaceZeros a = runSTUArray $ do
  thawed <- thaw a
  let end = (snd . bounds) a
  forM_ [0 .. end] $ \i -> do
    el <- readArray thawed i
    when (el == 0) $ do
      writeArray thawed i (-1)
  return thawed

main :: IO ()
main = return ()
