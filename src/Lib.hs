module Lib where

prelude :: String
prelude = unlines
  [ "  mapII: (f: (Int) -> Int, a: [Int]) -> [Int] = case (!a) [] otherwise f(h@a) :: mapII(f, t@a)"

  , "  mapIP: (f: (Int) -> Pitch, a: [Int]) -> [Pitch] = case (!a) [] otherwise f(h@a) :: mapIP(f, t@a)"

  , "  mapID: (f: (Int) -> Duration, a: [Int]) -> [Duration] = case (!a) [] otherwise f(h@a) :: mapID(f, t@a)"

  , "  mapIB: (f: (Int) -> Bool, a: [Int]) -> [Bool] = case (!a) [] otherwise f(h@a) :: mapIB(f, t@a)"

  , "  lenI: (xs: [Int]) -> Int = case(xs == []) 0 otherwise 1 + lenI(t@xs)"

  , "  lenB: (xs: [Bool]) -> Int = case(xs == []) 0 otherwise 1 + lenB(t@xs)"

  , "  concat: (x1: [Int], x2: [Int]) -> [Int] = case(!x1) x2 otherwise (h@x1 :: concat(t@x1, x2))"

  , "  filterI: (f: (Int) -> Bool, a: [Int]) -> [Int] = case (!a) [] otherwise { case (f(h@a)) h@a :: filterI(f, t@a) otherwise filterI(f, t@a) }"

  , "  foldrII: (f: (Int, Int) -> Int, a: Int, xs: [Int]) -> Int = case (!xs) a otherwise { foldrII(f, acc, t@xs) where acc: Int = f(a, h@xs) }"

  , "  sumI: (xs: [Int]) -> Int = foldrII(\\x: Int, y: Int -> Int: x + y, 0, xs)"

  , "  sequence: (start: Int, end: Int) -> [Int] = case (start >= end - 1) [end] otherwise start :: sequence(start + 1, end)"

  , "  length: (xs: [Int]) -> Int = case (!xs) 0 otherwise 1 + length(t@xs)"

  , "  last: (xs: [Int]) -> Int = case (length(xs) == 1) h@xs otherwise last(t@xs)"

  , "  init: (xs: [Int]) -> [Int] = case (length(xs) == 1) [] otherwise h@xs :: init(t@xs)"

  , "  reverse: (xs: [Int]) -> [Int] = case (!xs) [] otherwise last(xs) :: reverse(init(xs))"

  ]
  -- TODO: foldl
  -- TODO: cases for things other than `Int`s

