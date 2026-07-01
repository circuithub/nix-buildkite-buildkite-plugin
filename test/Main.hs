{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (Value(..))
import Data.Aeson.Types (parseMaybe, (.:))
import qualified Data.Text as T
import Data.Maybe (mapMaybe)
import qualified Data.Vector as V
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, (@?=))

import NixBuildkite (Config(..), defaultConfig, generatePipeline)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "nix-buildkite"
  [ testGroup "Independent jobs"
      [ testCase "BATCH_SIZE=2 produces 3 batches" testIndependentBatchCount
      , testCase "produces 5 total steps" testIndependentTotalSteps
      , testCase "default batch size produces single batch" testDefaultBatchSize
      ]
  , testGroup "Dependent jobs"
      [ testCase "jobA appears before jobB" testDependencyOrder
      , testCase "jobB has jobA in depends_on" testJobBDependsOnJobA
      , testCase "jobC has jobB in depends_on" testJobCDependsOnJobB
      ]
  , testGroup "Transitive dependencies"
      [ testCase "jobB depends on jobA through intermediate" testTransitiveDep
      ]
  , testGroup "Output format"
      [ testCase "all steps have required fields" testRequiredFields
      ]
  , testGroup "Batch splitting"
      [ testCase "each batch respects BATCH_SIZE limit" testBatchSizeLimit
      , testCase "batch sizes correctly distributed" testBatchDistribution
      ]
  , testGroup "Max steps"
      [ testCase "collapses to one job when exceeded" testMaxStepsCollapses
      , testCase "does not collapse at the limit" testMaxStepsAtLimit
      , testCase "no limit leaves the pipeline unchanged" testMaxStepsUnset
      ]
  ]

-- | Run the pipeline generator with the given batch size
runWithBatchSize :: Maybe Int -> FilePath -> IO [[Value]]
runWithBatchSize batchSize jobsFile = do
  let config = defaultConfig
        { configBatchSize = maybe (configBatchSize defaultConfig) id batchSize
        }
  generatePipeline config jobsFile

-- | Run the pipeline generator with a tweaked default config.
runWith :: (Config -> Config) -> FilePath -> IO [[Value]]
runWith tweak = generatePipeline (tweak defaultConfig)

-- | Get all steps from all batches
getAllSteps :: [[Value]] -> [Value]
getAllSteps = concat

-- | Get a field from a step
getField :: T.Text -> Value -> Maybe Value
getField field (Object obj) = parseMaybe (.: field) obj
getField _ _ = Nothing

-- | Get the label from a step
getLabel :: Value -> Maybe T.Text
getLabel step = case getField "label" step of
  Just (String t) -> Just t
  _ -> Nothing

-- | Get depends_on from a step
getDependsOn :: Value -> [T.Text]
getDependsOn step = case getField "depends_on" step of
  Just (Array arr) -> mapMaybe extractString (V.toList arr)
  _ -> []
  where
    extractString (String t) = Just t
    extractString _ = Nothing

-- | Get the command from a step
getCommand :: Value -> T.Text
getCommand step = case getField "command" step of
  Just (String t) -> t
  _ -> ""

-- | Get the key from a step
getKey :: Value -> Maybe T.Text
getKey step = case getField "key" step of
  Just (String t) -> Just t
  _ -> Nothing

-- | The fixed key of the single "build everything" job emitted on collapse.
buildAllKey :: T.Text
buildAllKey = "nix-buildkite-build-all"

-- | A pipeline is "collapsed" when it is a single build-everything step.
isCollapsed :: [[Value]] -> Bool
isCollapsed batches = case getAllSteps batches of
  [step] -> getKey step == Just buildAllKey
  _      -> False

independentJobsFile :: FilePath
independentJobsFile = "test/fixtures/independent-jobs.nix"

dependentJobsFile :: FilePath
dependentJobsFile = "test/fixtures/dependent-jobs.nix"

transitiveDepsFile :: FilePath
transitiveDepsFile = "test/fixtures/transitive-deps.nix"

-- Tests for independent jobs

testIndependentBatchCount :: IO ()
testIndependentBatchCount = do
  batches <- runWithBatchSize (Just 2) independentJobsFile
  assertEqual "should produce 3 batches" 3 (length batches)

testIndependentTotalSteps :: IO ()
testIndependentTotalSteps = do
  batches <- runWithBatchSize (Just 2) independentJobsFile
  let steps = getAllSteps batches
  assertEqual "should produce 5 steps" 5 (length steps)

testDefaultBatchSize :: IO ()
testDefaultBatchSize = do
  batches <- runWithBatchSize Nothing independentJobsFile
  assertEqual "should produce 1 batch with default size" 1 (length batches)

-- Tests for dependent jobs

testDependencyOrder :: IO ()
testDependencyOrder = do
  batches <- runWithBatchSize (Just 2) dependentJobsFile
  let steps = getAllSteps batches
      stepLabels = mapMaybe getLabel steps
      jobAIndex = findIndex "jobA" stepLabels
      jobBIndex = findIndex "jobB" stepLabels
  assertBool "jobA should appear before jobB" (jobAIndex <= jobBIndex)
  where
    findIndex :: T.Text -> [T.Text] -> Int
    findIndex needle xs =
      case filter (\(_, x) -> needle `T.isInfixOf` x) (zip [0..] xs) of
        ((n, _):_) -> n
        [] -> maxBound

testJobBDependsOnJobA :: IO ()
testJobBDependsOnJobA = do
  batches <- runWithBatchSize (Just 2) dependentJobsFile
  let steps = getAllSteps batches
      jobBSteps = filter (\s -> getLabel s == Just "jobB") steps
  case jobBSteps of
    [jobB] -> assertBool "jobB should depend on jobA" $
                any ("jobA" `T.isInfixOf`) (getDependsOn jobB)
    _ -> assertBool "should find exactly one jobB" False

testJobCDependsOnJobB :: IO ()
testJobCDependsOnJobB = do
  batches <- runWithBatchSize (Just 2) dependentJobsFile
  let steps = getAllSteps batches
      jobCSteps = filter (\s -> getLabel s == Just "jobC") steps
  case jobCSteps of
    [jobC] -> assertBool "jobC should depend on jobB" $
                any ("jobB" `T.isInfixOf`) (getDependsOn jobC)
    _ -> assertBool "should find exactly one jobC" False

-- Tests for transitive dependencies through non-job intermediates

testTransitiveDep :: IO ()
testTransitiveDep = do
  batches <- runWithBatchSize Nothing transitiveDepsFile
  let steps = getAllSteps batches
      jobBSteps = filter (\s -> getLabel s == Just "jobB") steps
  case jobBSteps of
    [jobB] -> assertBool "jobB should transitively depend on jobA" $
                any ("jobA" `T.isInfixOf`) (getDependsOn jobB)
    _ -> assertBool "should find exactly one jobB" False

-- Tests for output format

testRequiredFields :: IO ()
testRequiredFields = do
  batches <- runWithBatchSize Nothing independentJobsFile
  let steps = getAllSteps batches
      hasAllFields step =
        all (\f -> getField f step /= Nothing) ["label", "command", "key", "depends_on"]
      missingFields = filter (not . hasAllFields) steps
  assertEqual "all steps should have required fields" 0 (length missingFields)

-- Tests for batch splitting

testBatchSizeLimit :: IO ()
testBatchSizeLimit = do
  batches <- runWithBatchSize (Just 2) independentJobsFile
  let batchSizes = map length batches
      maxSize = maximum batchSizes
  assertBool "no batch should exceed BATCH_SIZE" (maxSize <= 2)

testBatchDistribution :: IO ()
testBatchDistribution = do
  batches <- runWithBatchSize (Just 2) independentJobsFile
  let batchSizes = reverse $ sort $ map length batches
  batchSizes @?= [2, 2, 1]
  where
    sort = foldr insert []
    insert x [] = [x]
    insert x (y:ys)
      | x <= y = x : y : ys
      | otherwise = y : insert x ys

-- Tests for the max-steps collapse threshold

-- | 5 independent jobs with a limit of 3 collapse into one build-everything job.
testMaxStepsCollapses :: IO ()
testMaxStepsCollapses = do
  batches <- runWith (\c -> c { configMaxSteps = Just 3 }) independentJobsFile
  assertEqual "should be a single batch" 1 (length batches)
  case getAllSteps batches of
    [step] -> do
      assertEqual "collapsed step key" (Just buildAllKey) (getKey step)
      let cmd = getCommand step
      assertBool "builds with --keep-going" ("--keep-going" `T.isInfixOf` cmd)
      assertBool "references every job" $
        all (`T.isInfixOf` cmd) ["job1", "job2", "job3", "job4", "job5"]
      assertEqual "has no dependencies" [] (getDependsOn step)
    steps -> assertEqual "should collapse to exactly one step" 1 (length steps)

-- | At the limit (5 jobs, limit 5) the pipeline is not collapsed.
testMaxStepsAtLimit :: IO ()
testMaxStepsAtLimit = do
  batches <- runWith (\c -> c { configMaxSteps = Just 5 }) independentJobsFile
  assertBool "should not collapse at the limit" (not (isCollapsed batches))
  assertEqual "should keep all 5 steps" 5 (length (getAllSteps batches))

-- | With no limit set, behaviour is unchanged.
testMaxStepsUnset :: IO ()
testMaxStepsUnset = do
  batches <- runWith id independentJobsFile
  assertBool "should not collapse" (not (isCollapsed batches))
  assertEqual "should keep all 5 steps" 5 (length (getAllSteps batches))
