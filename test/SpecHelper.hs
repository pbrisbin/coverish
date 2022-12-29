module SpecHelper
    ( buildTrace
    , withFileTree
    , module X
    ) where

import Prelude as X

import Coverish as X
import Data.Text as X (Text)
import Test.Hspec as X

import Control.Monad (forM_)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)

import qualified Data.Text as T
import qualified Data.Text.IO as T

buildTrace :: [Text] -> Text
buildTrace = T.unlines

withFileTree :: [(FilePath, Text)] -> (FilePath -> IO a) -> IO a
withFileTree paths act = withSystemTempDirectory "" $ \dir -> do
    forM_ paths $ \(path, content) -> do
        let absolute = dir </> path
        createDirectoryIfMissing True $ takeDirectory absolute
        T.writeFile absolute content

    act dir
