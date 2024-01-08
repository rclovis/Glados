module Ast.Utils (takeUntil) where

takeUntil :: (a -> Bool) -> [a] -> ([a], [a])
takeUntil _ [] = ([], [])
takeUntil f (x : xs)
  | f x = ([], xs)
  | otherwise = (x : ys, zs)
  where
    (ys, zs) = takeUntil f xs
