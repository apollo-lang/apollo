
--------------------------------------------------------------------------
-- Lib: Apollo standard library prelude
--------------------------------------------------------------------------

module Lib (
  prelude
) where

prelude :: String
prelude = unlines
  [ "  mapII: (f: (Int) -> Int, a: [Int]) -> [Int] = case (!a) [] otherwise f(h@a) :: mapII(f, t@a)"

  , "  mapIP: (f: (Int) -> Pitch, a: [Int]) -> [Pitch] = case (!a) [] otherwise f(h@a) :: mapIP(f, t@a)"

  , "  mapID: (f: (Int) -> Duration, a: [Int]) -> [Duration] = case (!a) [] otherwise f(h@a) :: mapID(f, t@a)"

  , "  mapIB: (f: (Int) -> Bool, a: [Int]) -> [Bool] = case (!a) [] otherwise f(h@a) :: mapIB(f, t@a)"

  , "  mapPL: (f: ([Pitch]) -> [Pitch], a: [[Pitch]]) -> [[Pitch]] = case (!a) [] otherwise f(h@a) :: mapPL(f, t@a)"

  , "  lengthI: (xs: [Int]) -> Int = case(xs == []) 0 otherwise 1 + lengthI(t@xs)"

  , "  lengthB: (xs: [Bool]) -> Int = case(xs == []) 0 otherwise 1 + lengthB(t@xs)"

  , "  lengthP: (xs: [Pitch]) -> Int = case(xs == []) 0 otherwise 1 + lengthP(t@xs)"

  , "  concatI: (x1: [Int], x2: [Int]) -> [Int] = case (!x1) x2 otherwise (h@x1 :: concatI(t@x1, x2))"

  , "  concatP: (x1: [Pitch], x2: [Pitch]) -> [Pitch] = case (!x1) x2 otherwise (h@x1 :: concatP(t@x1, x2))"

  , "  filterI: (f: (Int) -> Bool, a: [Int]) -> [Int] = case (!a) [] otherwise { case (f(h@a)) h@a :: filterI(f, t@a) otherwise filterI(f, t@a) }"

  , "  foldrII: (f: (Int, Int) -> Int, a: Int, xs: [Int]) -> Int = case (!xs) a otherwise { foldrII(f, acc, t@xs) where acc: Int = f(a, h@xs) }"

  , "  sumI: (xs: [Int]) -> Int = foldrII(\\x: Int, y: Int -> Int: x + y, 0, xs)"

  , "  sequence: (start: Int, end: Int) -> [Int] = case (start >= end) [] otherwise start :: sequence(start + 1, end)"

  , "  zip: (a: [Pitch], b: [Duration]) -> [Atom] = case (!a || !b) [] otherwise (h@a, h@b) :: zip(t@a, t@b)"

  , "  replicateI: (xs: [Int], n: Int) -> [Int] = case (n == 0) [] otherwise concatI(xs, replicateI(xs, n - 1))"

  , "  replicateP: (xs: [Pitch], n: Int) -> [Pitch] = case (n == 0) [] otherwise concatP(xs, replicateP(xs, n - 1))"

  , "  uniform: (d: Duration, n: Int) -> [Duration] = case (n == 0) [] otherwise d :: uniform(d, n - 1)"

  , "  lastI: (xs: [Int]) -> Int = case (lengthI(xs) == 1) h@xs otherwise lastI(t@xs)"

  , "  lastP: (xs: [Pitch]) -> Pitch = case (lengthP(xs) == 1) h@xs otherwise lastP(t@xs)"

  , "  initI: (xs: [Int]) -> [Int] = case (lengthI(xs) == 1) [] otherwise h@xs :: initI(t@xs)"

  , "  initP: (xs: [Pitch]) -> [Pitch] = case (lengthP(xs) == 1) [] otherwise h@xs :: initP(t@xs)"

  , "  reverseI: (xs: [Int]) -> [Int] = case (!xs) [] otherwise lastI(xs) :: reverseI(initI(xs))"

  , "  reverseP: (xs: [Pitch]) -> [Pitch] = case (!xs) [] otherwise lastP(xs) :: reverseP(initP(xs))"

  , "  intercalateI: (a: [Int], b: [Int]) -> [Int] = case (!a || !b) [] otherwise concatI([h@a, h@b], intercalateI(t@a, t@b))"

  , "  intercalateP: (a: [Pitch], b: [Pitch]) -> [Pitch] = case (!a || !b) [] otherwise concatP([h@a, h@b], intercalateP(t@a, t@b))"

  , "  concatMapI: (xs: [Int], f: (Int) -> Int, c: Int) -> [Int] = case (c <= 0) xs otherwise concatI(xs, concatMapI(mapII(f, xs), f, c-1))"

  , "  flattenP: (p: [[Pitch]]) -> [Pitch] = case (!p) [] otherwise concatP(h@p, flattenP(t@p))"

  ]
