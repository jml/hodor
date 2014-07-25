{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tests.Generators where

import Control.Monad (liftM)
import Data.Time (Day, fromGregorian, gregorianMonthLength)
import Test.QuickCheck

import Hodor.Parser (parseTodoLine)
import Hodor.Types


arbitraryPriorityLetter :: Gen Char
arbitraryPriorityLetter = choose ('A', 'Z')


arbitraryPriority :: Gen Priority
arbitraryPriority = liftM unsafeMakePriority arbitraryPriorityLetter


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


sampleTodoLines :: [String]
sampleTodoLines = [
  "x 2013-10-12 2013-09-20 A done task +some-project",
  "(B) 2013-09-27 Wipe mould off bathroom ceiling +condensation @home",
  "2013-09-21 Email John arranging time to catch up @online +some-project",
  "Nothing to see here",
  "(A) Do taxes"
  ]

arbitraryTodoLines :: Gen String
arbitraryTodoLines = elements sampleTodoLines


instance Arbitrary TodoItem where
  arbitrary = do
    line <- arbitraryTodoLines
    case parseTodoLine line of
      Left e -> error (show e)
      Right r -> return r
