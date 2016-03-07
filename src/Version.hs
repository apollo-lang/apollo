module Version (
  version
) where

import qualified Paths_apollo as P (version)
import Data.Version (showVersion)

version :: String
version = showVersion P.version

