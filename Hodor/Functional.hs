module Hodor.Functional where


-- Do something to Left, leave Right untouched.
onLeft :: (a -> b) -> Either a c -> Either b c
onLeft f (Left x)  = Left (f x)
onLeft _ (Right x) = Right x

