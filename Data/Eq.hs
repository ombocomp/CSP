module Data.Eq where

-- |@Eq@-analogue of @Data.Ord.comparing@.
equating :: Eq b => (a -> b) -> a -> a -> Bool
equating f x y = f x == f y