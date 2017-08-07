module Coverish.Trace.Lookup
    ( TraceLookup
    , buildTraceLookup
    , tracePaths
    , executedInTrace
    ) where

import Control.Monad ((<=<))
import Data.Either (rights)
import System.Directory (getCurrentDirectory, withCurrentDirectory)
import System.FilePath ((</>), splitFileName)

import qualified Control.Exception as E
import qualified Data.Map as M

import Coverish.Trace (Execution(..), Trace(..))

-- | Re-structured @'Trace'@ information, to optimize our lookups
newtype TraceLookup = TraceLookup
    { tlMap :: M.Map FilePath (M.Map Int Int)
    }
    deriving (Eq, Show)

-- | Construct a @'TraceLookup'@
--
-- Requires @'IO'@ because the paths in a trace need to be validated,
-- normalized, and canonicalized as absolute in order to de-duplicate.
--
buildTraceLookup :: Trace -> IO TraceLookup
buildTraceLookup (Trace es) =
    TraceLookup . M.fromListWith combine . rights <$> mapM executionToPair es
  where
    combine :: M.Map Int Int -> M.Map Int Int -> M.Map Int Int
    combine = M.unionWith (+)

-- | All (normalized) paths in the trace-lookup
tracePaths :: TraceLookup -> [FilePath]
tracePaths = M.keys . tlMap

-- | Was this path/line executed in the trace? If so, how many times?
executedInTrace :: FilePath -> Int -> TraceLookup -> Maybe Int
executedInTrace path line = M.lookup line <=< M.lookup path . tlMap

executionToPair :: Execution -> IO (Either E.IOException (FilePath, M.Map Int Int))
executionToPair ex = E.try $ do
    let (dir, name) = splitFileName $ exPath ex

    path <- withCurrentDirectory dir $ (</> name) <$> getCurrentDirectory

    return (path, M.fromList [(exLine ex, 1)])
