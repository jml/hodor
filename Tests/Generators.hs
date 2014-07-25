{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tests.Generators where

import Control.Monad (liftM)
import Data.Time (Day, fromGregorian, gregorianMonthLength)
import Test.QuickCheck

import Hodor.Types


arbitraryPriority :: Gen Priority
arbitraryPriority = liftM unsafeMakePriority $ choose ('A', 'Z')


instance Arbitrary Priority where
  arbitrary = frequency [(5, arbitraryPriority), (1, return noPriority)]


instance Arbitrary Day where
  arbitrary = do
    year <- arbitrary
    month <- arbitraryMonth
    day <- arbitraryDay year month
    return $ fromGregorian year month day

arbitraryMonth :: Gen Int
arbitraryMonth = choose (1, 12)

arbitraryDay :: Integer -> Int -> Gen Int
arbitraryDay y m = choose (1, gregorianMonthLength y m)
