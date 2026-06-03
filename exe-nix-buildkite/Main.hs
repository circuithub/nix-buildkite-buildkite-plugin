{-# language OverloadedStrings #-}

module Main ( main ) where

import Data.Aeson ( encode, object, (.=) )
import Data.Maybe ( fromMaybe, listToMaybe )
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Environment ( getArgs, lookupEnv )
import Text.Read ( readMaybe )

import NixBuildkite ( Config(..), defaultConfig, generatePipeline )

main :: IO ()
main = do
  jobsExpr <- fromMaybe "./jobs.nix" . listToMaybe <$> getArgs

  postBuildHook <- lookupEnv "POST_BUILD_HOOK"

  skipAlreadyBuilt <- do
    e <- lookupEnv "SKIP_ALREADY_BUILT"
    pure $ case e of
      Just "true" -> True
      Just "false" -> False
      Just _ -> error "SKIP_ALREADY_BUILT only accepts 'true' or 'false'."
      Nothing -> False

  batchSize <- do
    e <- lookupEnv "BATCH_SIZE"
    pure $ case e of
      Nothing -> configBatchSize defaultConfig
      Just s -> fromMaybe (error "BATCH_SIZE must be a positive integer") (readMaybe s)

  -- If set, collapse to a single "build everything" job once the pipeline would
  -- exceed this many steps. Unset means no limit.
  maxSteps <- do
    e <- lookupEnv "MAX_STEPS"
    pure $ case e of
      Nothing -> Nothing
      Just s -> Just $ fromMaybe (error "MAX_STEPS must be a positive integer") (readMaybe s)

  -- If set, collapse to a single "build everything" job once the peak number of
  -- jobs that could run concurrently (the job graph's maximum antichain)
  -- exceeds this. Unset means no limit.
  maxConcurrency <- do
    e <- lookupEnv "MAX_CONCURRENCY"
    pure $ case e of
      Nothing -> Nothing
      Just s -> Just $ fromMaybe (error "MAX_CONCURRENCY must be a positive integer") (readMaybe s)

  let config = Config
        { configPostBuildHook = postBuildHook
        , configSkipAlreadyBuilt = skipAlreadyBuilt
        , configBatchSize = batchSize
        , configMaxSteps = maxSteps
        , configMaxConcurrency = maxConcurrency
        }

  batches <- generatePipeline config jobsExpr

  -- Output one JSON object per line
  mapM_ (\batch -> BL.putStrLn $ encode $ object [ "steps" .= batch ]) batches
