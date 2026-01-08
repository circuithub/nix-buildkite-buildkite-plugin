{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (Value(..), decode, (.:))
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Data.Maybe (mapMaybe)
import qualified Data.Vector as V
import System.Environment (setEnv, lookupEnv, unsetEnv)
import System.Process (readProcess)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, (@?=))

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
  ]

-- | Run nix-buildkite with the given jobs file and batch size
runNixBuildkite :: Maybe Int -> FilePath -> IO [Value]
runNixBuildkite batchSize jobsFile = do
  -- Save and set BATCH_SIZE
  oldBatchSize <- lookupEnv "BATCH_SIZE"
  case batchSize of
    Just n -> setEnv "BATCH_SIZE" (show n)
    Nothing -> unsetEnv "BATCH_SIZE"

  -- Run the executable
  output <- readProcess "cabal" ["run", "-v0", "nix-buildkite", "--", jobsFile] ""

  -- Restore BATCH_SIZE
  case oldBatchSize of
    Just v -> setEnv "BATCH_SIZE" v
    Nothing -> unsetEnv "BATCH_SIZE"

  -- Parse each line as JSON
  let batches = mapMaybe (decode . BL.pack) (lines output)
  return batches

-- | Extract steps from a batch
getSteps :: Value -> [Value]
getSteps (Object obj) = case parseMaybe (.: "steps") obj of
  Just (Array arr) -> V.toList arr
  _ -> []
getSteps _ = []

-- | Get all steps from all batches
getAllSteps :: [Value] -> [Value]
getAllSteps = concatMap getSteps

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

independentJobsFile :: FilePath
independentJobsFile = "test/fixtures/independent-jobs.nix"

dependentJobsFile :: FilePath
dependentJobsFile = "test/fixtures/dependent-jobs.nix"

transitiveDepsFile :: FilePath
transitiveDepsFile = "test/fixtures/transitive-deps.nix"

-- Tests for independent jobs

testIndependentBatchCount :: IO ()
testIndependentBatchCount = do
  batches <- runNixBuildkite (Just 2) independentJobsFile
  assertEqual "should produce 3 batches" 3 (length batches)

testIndependentTotalSteps :: IO ()
testIndependentTotalSteps = do
  batches <- runNixBuildkite (Just 2) independentJobsFile
  let steps = getAllSteps batches
  assertEqual "should produce 5 steps" 5 (length steps)

testDefaultBatchSize :: IO ()
testDefaultBatchSize = do
  batches <- runNixBuildkite Nothing independentJobsFile
  assertEqual "should produce 1 batch with default size" 1 (length batches)

-- Tests for dependent jobs

testDependencyOrder :: IO ()
testDependencyOrder = do
  batches <- runNixBuildkite (Just 2) dependentJobsFile
  let output = unlines $ map (BL.unpack . BL.pack . show) batches
      jobALine = findFirstLineWith "jobA" output :: Int
      jobBLine = findFirstLineWith "jobB" output :: Int
  assertBool "jobA should appear before jobB" (jobALine <= jobBLine)
  where
    findFirstLineWith :: String -> String -> Int
    findFirstLineWith needle text =
      case filter (needle `isInfixOf`) (zip [1..] (lines text)) of
        ((n, _):_) -> n
        [] -> maxBound
    isInfixOf :: String -> (Int, String) -> Bool
    isInfixOf needle (_, line) = T.pack needle `T.isInfixOf` T.pack line

testJobBDependsOnJobA :: IO ()
testJobBDependsOnJobA = do
  batches <- runNixBuildkite (Just 2) dependentJobsFile
  let steps = getAllSteps batches
      jobBSteps = filter (\s -> getLabel s == Just "jobB") steps
  case jobBSteps of
    [jobB] -> assertBool "jobB should depend on jobA" $
                any ("jobA" `T.isInfixOf`) (getDependsOn jobB)
    _ -> assertBool "should find exactly one jobB" False

testJobCDependsOnJobB :: IO ()
testJobCDependsOnJobB = do
  batches <- runNixBuildkite (Just 2) dependentJobsFile
  let steps = getAllSteps batches
      jobCSteps = filter (\s -> getLabel s == Just "jobC") steps
  case jobCSteps of
    [jobC] -> assertBool "jobC should depend on jobB" $
                any ("jobB" `T.isInfixOf`) (getDependsOn jobC)
    _ -> assertBool "should find exactly one jobC" False

-- Tests for transitive dependencies through non-job intermediates

testTransitiveDep :: IO ()
testTransitiveDep = do
  batches <- runNixBuildkite Nothing transitiveDepsFile
  let steps = getAllSteps batches
      jobBSteps = filter (\s -> getLabel s == Just "jobB") steps
  case jobBSteps of
    [jobB] -> assertBool "jobB should transitively depend on jobA" $
                any ("jobA" `T.isInfixOf`) (getDependsOn jobB)
    _ -> assertBool "should find exactly one jobB" False

-- Tests for output format

testRequiredFields :: IO ()
testRequiredFields = do
  batches <- runNixBuildkite Nothing independentJobsFile
  let steps = getAllSteps batches
      hasAllFields step =
        all (\f -> getField f step /= Nothing) ["label", "command", "key", "depends_on"]
      missingFields = filter (not . hasAllFields) steps
  assertEqual "all steps should have required fields" 0 (length missingFields)

-- Tests for batch splitting

testBatchSizeLimit :: IO ()
testBatchSizeLimit = do
  batches <- runNixBuildkite (Just 2) independentJobsFile
  let batchSizes = map (length . getSteps) batches
      maxSize = maximum batchSizes
  assertBool "no batch should exceed BATCH_SIZE" (maxSize <= 2)

testBatchDistribution :: IO ()
testBatchDistribution = do
  batches <- runNixBuildkite (Just 2) independentJobsFile
  let batchSizes = reverse $ sort $ map (length . getSteps) batches
  batchSizes @?= [2, 2, 1]
  where
    sort = foldr insert []
    insert x [] = [x]
    insert x (y:ys)
      | x <= y = x : y : ys
      | otherwise = y : insert x ys
