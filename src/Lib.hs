module Lib where

prelude :: String
prelude = unlines
  [ "map: (f: (Int) -> Int, a: [Int]) -> [Int] = case (!a) [] otherwise f(h@a) :: map(f, t@a)"
  , "len: (xs: [Int]) -> Int = case(xs == []) 0 otherwise 1 + len(t@xs)"
  , "concat: (x1: [Int], x2: [Int]) -> [Int] = case(!x1) x2 otherwise (h@x1 :: concat(t@x1, x2))"
  ]
