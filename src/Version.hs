--  {-# LANGUAGE TemplateHaskell #-}

--  module Version (
--    version
--  ) where
--  
--  import Distribution.PackageDescription.TH
--  
--  -- TODO :: [Int]
--  
--  version :: String
--  version = $(packageVariable (pkgVersion . package))

module Version (
  version
) where

import qualified Paths_apollo as P (version) 
import Data.Version (showVersion)

version = showVersion P.version

