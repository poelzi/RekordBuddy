module RekordBuddy.Configuration (Configuration(..)) where
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (ap)
import Data.Text.Prettyprint.Doc
import Numeric (showHex)
import Data.Char (isAscii, isPrint, ord)

import RekordBuddy.Types (ManyToMany, EntityMap)

-- | Information required to build full paths to files or namespace, or parameters that apply to all files
-- like nesting
data Configuration = Configuration
   { configurationRoot :: Text
   , configurationNamespaces :: [Text]
   , defaultVersion :: Bool
   , configurationVersion :: Int
   , configurationManyToMany :: [ManyToMany]
   , configurationEntities :: EntityMap
   , minVersion :: Int
   , nesting :: Int
   , configurationCocoaWrappers :: Bool
   }
 deriving Show

