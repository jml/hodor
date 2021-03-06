module Hodor.Functional where

import Control.Exception (AssertionFailed(AssertionFailed), throw)

-- Do something to Left, leave Right untouched.
onLeft :: (a -> b) -> Either a c -> Either b c
onLeft f (Left x)  = Left (f x)
onLeft _ (Right x) = Right x


-- Unwrap a left that we know to be a Left
unleft :: Either a b -> a
unleft (Left x) = x
unleft _ = throw $ AssertionFailed $ "Expected Left, got Right"


-- All the predicates are true of x
andP :: ([a -> Bool]) -> a -> Bool
andP ps x = and (map ($ x) ps)
