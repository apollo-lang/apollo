module Version (
  version
) where

import qualified Paths_apollo as P (version)
import Data.Version (showVersion)

version = showVersion P.version

