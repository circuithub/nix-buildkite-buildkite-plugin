{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language NumericUnderscores #-}
{-# language TemplateHaskell #-}

module NixBuildkite
  ( -- * Configuration
    Config(..)
  , defaultConfig
    -- * Pipeline generation
  , generatePipeline
    -- * Utilities (exported for testing)
  , chunksOf
  , stepify
  ) where

-- algebraic-graphs
import Algebra.Graph.AdjacencyMap ( AdjacencyMap, edge, empty, hasVertex, overlay, overlays, postSet, preSet, vertices, vertexSet )
import qualified Algebra.Graph.AdjacencyMap.Algorithm as AMA

-- aeson
import Data.Aeson ( Value(..), (.=), object )

-- attoparsec
import Data.Attoparsec.Text ( parseOnly )

-- base
import Data.Char ( isAlphaNum )
import Data.Maybe ( fromMaybe, mapMaybe )
import Data.Foldable ( toList )
import Data.Traversable ( for )
import Data.List ( partition, intercalate )
import qualified Data.List
import qualified Prelude
import Prelude hiding ( readFile )
import System.IO ( hPutStrLn, hGetContents, stderr )
import Text.Printf ( printf )
import qualified Data.List as List
import System.Exit ( ExitCode(..) )

-- clock
import System.Clock

-- containers
import Data.Containers.ListUtils ( nubOrd )
import qualified Data.Map as Map
import qualified Data.Set as S

-- filepath
import System.FilePath ( takeFileName, takeBaseName )

-- nix-derivation
import Nix.Derivation ( Derivation(..), parseDerivation )

-- process
import System.Process hiding (env)

-- template-haskell
import Language.Haskell.TH ( litE, stringL, runIO )
import Language.Haskell.TH.Syntax ( addDependentFile )

-- text
import Data.Text ( Text, pack, unpack )
import Data.Text.IO ( readFile )


-- | Configuration for pipeline generation.
data Config = Config
  { configPostBuildHook :: Maybe FilePath
    -- ^ Optional path to a post-build hook script.
  , configSkipAlreadyBuilt :: Bool
    -- ^ If True, skip derivations that are already built.
  , configBatchSize :: Int
    -- ^ Maximum number of steps per batch (Buildkite limit is 500).
  , configMaxSteps :: Maybe Int
    -- ^ If set, and the pipeline would produce more than this many steps,
    -- collapse the whole pipeline into a single job that builds everything
    -- (see 'generatePipelineFromDrvPaths'). 'Nothing' means no limit.
  } deriving (Show, Eq)

-- | Default configuration with sensible defaults.
defaultConfig :: Config
defaultConfig = Config
  { configPostBuildHook = Nothing
  , configSkipAlreadyBuilt = False
  , configBatchSize = 450
  , configMaxSteps = Nothing
  }

-- | Sometimes nix will return stuff that looks like @/nix/store/asdfasdf-foo.drv!doc@.
-- This is the syntax for showing that we are talking about a particular output.
-- We do not want this in our drvs since we want to open the file and there is no such file when the
-- bang is included.
removeBang :: String -> String
removeBang str = Data.List.takeWhile (/='!') str

-- | Generate pipeline batches from a jobs.nix file.
-- Returns a list of batches, where each batch is a list of Buildkite step values.
generatePipeline :: Config -> FilePath -> IO [[Value]]
generatePipeline config jobsExpr = do
  -- Run nix-instantiate on the jobs expression to instantiate .drvs for all
  -- things that may need to be built.
  inputDrvPaths <- nubOrd . map removeBang <$> nixInstantiate jobsExpr

  -- Get the list of derivations that will be built, which may include drvs not in inputDrvPaths
  pathsToBuild <- if configSkipAlreadyBuilt config
    then nixBuildDryRun inputDrvPaths
    else pure inputDrvPaths

  -- Filter our inputDrvs down to just those that will be built (if the skip already built flag is set)
  let inputDrvPathsToBuild = S.toList $ S.fromList inputDrvPaths `S.intersection` S.fromList pathsToBuild

  generatePipelineFromDrvPaths config inputDrvPathsToBuild

-- | Generate pipeline batches from a list of derivation paths.
-- This is the core logic, useful for testing without needing nix-instantiate.
generatePipelineFromDrvPaths :: Config -> [FilePath] -> IO [[Value]]
generatePipelineFromDrvPaths config inputDrvPathsToBuild = do
  let postBuildHook = postBuildHookArgs config

  -- Build an association list of a job name and the derivation that should be
  -- realised for that job.
  drvs <- for inputDrvPathsToBuild \drvPath -> do
    fmap (parseOnly parseDerivation) (readFile drvPath) >>= \case
      Left _ ->
        -- We couldn't parse the derivation to get a name, so we'll just use the
        -- derivation name.
        return (pack (takeFileName drvPath), drvPath)

      Right drv ->
        case Map.lookup "name" (env drv) of
          Nothing ->
            -- There was no 'name' environment variable, so we'll just use the
            -- derivation name.
            return (pack (takeFileName drvPath), drvPath)

          Just name ->
            return (name, drvPath)

  g <- foldr (\(_, drv) m -> m >>= \g' -> add g' drv) (pure empty) drvs

  -- A subset of our derivations which we want to see in Buildkite.
  -- These are the derivations we get as a result of running `nix-instantiate`
  let jobSet = S.fromList $ map snd drvs

  -- Calculate the dependencies for each job.
  --
  -- Jobs may depend on other jobs through non-job derivations, eg,
  -- jobA -> intermediate1 -> intermediate2 -> jobB
  -- means that jobA depends on jobB.
  --
  -- Conceptually, we want:
  --   transitive closure of g → restrict to jobs → transitive reduction
  -- This gives us direct job-to-job dependencies, collapsing through non-job intermediates
  -- (e.g., jobB -> intermediate -> jobA becomes jobB -> jobA) but without redundant edges
  -- (e.g., if jobC -> jobB -> jobA, we don't want jobC -> jobA since Buildkite handles that).
  --
  -- To compute this would require taking the transitive closure of the graph,
  -- but that would end up with something O(V^2). And we cannot have a quadratic step.
  --
  -- Writing out the definition explicitly:
  --   Deriviation A depends on Job B iff
  --    there is a path starting at A consisting of zero or more non-job edges
  --    and then an edge leading to B.
  --
  -- This leads to a recursive algorithm:
  --   - (Base case): direct dependency between A and B
  --   - (Inductive step): A depends on some derivation C and C depends on Job B.
  --
  -- We memoize this with a lazy Map (NB: a strict map would diverge).
  -- This terminates if we don't have cycles.
  let depsOf v = fromMaybe S.empty $ Map.lookup v depsMap
        where
          depsMap = Map.fromList
            [(v', us) |
              v' <- S.toList (vertexSet g),
              let nexts = S.toList $ postSet v' g,
              let (ins, outs) = partition (`S.member` jobSet) nexts,
              let us = S.unions $ S.fromList ins : map depsOf outs
            ]

  let jobGraph :: AdjacencyMap FilePath
      jobGraph = overlay (vertices $ S.toList jobSet) $
        overlays
          [ overlays [edge dep job | dep <- S.toList (depsOf job)]  -- edge from dependency to dependent
          | job <- S.toList jobSet
          ]

  -- Topological sort: dependencies come before dependents.
  -- For edge (A -> B), topSort returns A before B. Our edges go from dependency to dependent,
  -- so dependencies come first in the result.
  --
  -- See Note [Pipeline batching]
  let sortedDrvPaths = case AMA.topSort jobGraph of
        Left depCycle -> error $ "Dependency cycle detected: " ++ unwords (toList depCycle)
        Right sorted -> sorted

  -- Create a map from drvPath to label for quick lookup
  let drvLabels = Map.fromList [(drvPath, label) | (label, drvPath) <- drvs]

  -- Map sorted paths back to (label, path) pairs
  let sortedDrvs = mapMaybe (\drvPath -> fmap (\label -> (label, drvPath)) (Map.lookup drvPath drvLabels)) sortedDrvPaths

  let step :: Text -> FilePath -> Value
      step label drvPath =
        object
          [ "label" .= unpack label
          , "command" .= String (pack $ unwords $ [ "nix-store" ] <> postBuildHook <> [ "-r", drvPath ])
          , "key" .= stepify drvPath
          , "depends_on" .= dependencies
          ]
        where
          dependencies = map stepify $ S.toList $ preSet drvPath jobGraph

  let steps = map (uncurry step) sortedDrvs
      numSteps = length sortedDrvs

  -- Decide whether to collapse the whole pipeline into a single "build
  -- everything" job. See Note [Collapsing large pipelines].
  let collapseReason :: Maybe String
      collapseReason
        | Just maxSteps <- configMaxSteps config
        , numSteps > maxSteps
        = Just $ printf "%d steps exceeds the max-steps limit of %d" numSteps maxSteps

        | otherwise
        = Nothing

  case collapseReason of
    -- Collapsed: a single job (its own single batch) that builds everything.
    Just reason -> return [ [ bigStep config reason sortedDrvs ] ]
    -- Normal: one step per job, split into batches.
    Nothing     -> return $ chunksOf (configBatchSize config) steps


-- | Split a list into chunks of at most n elements.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | The extra @nix-store@ arguments needed to run the configured post-build
-- hook, or @[]@ if none is configured.
postBuildHookArgs :: Config -> [String]
postBuildHookArgs config = case configPostBuildHook config of
  Nothing   -> []
  Just path -> [ "--post-build-hook", path ]

{- Note [Collapsing large pipelines]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When a pipeline would be too large ('configMaxSteps'), we replace the per-job
steps with a single "build everything" job ('bigStep'). That job realises every
requested derivation in one go with @--keep-going@, so a failure anywhere does
not abort the rest, and (when a post-build hook is configured) each derivation
that succeeds is uploaded to the cache as it completes. @nix-store --keep-going@
exits non-zero if anything failed, so the big job goes red.

The shell logic for that job lives in @scripts/build-all.sh@ (embedded as
'buildAllScript'); 'bigStepCommand' just prepends the derivation paths as
arguments. That script also tidies up nix's noisy failure output — see its
header comment.

This enables a recovery workflow: re-run the pipeline with skip-already-built
enabled. @nix-build --dry-run@ then reports only the failures and their blocked
dependents, which is a much smaller set that typically drops back under the
limit — so the re-run yields granular per-job steps showing exactly what failed,
at the cost of one re-evaluation. -}

-- | The @build-all.sh@ script, embedded at compile time. Keeping it as a real
-- script file — rather than a string assembled in Haskell — means it can be
-- read, shellchecked and edited as ordinary bash. It takes the derivations to
-- build as positional arguments and reads @NBK_POST_BUILD_HOOK@ from the
-- environment; 'bigStepCommand' supplies both.
--
-- 'addDependentFile' makes GHC recompile this module when the script changes.
buildAllScript :: Text
buildAllScript = pack $(do
    let scriptPath = "scripts/build-all.sh"
    addDependentFile scriptPath
    contents <- runIO (Prelude.readFile scriptPath)
    litE (stringL contents))

-- | Build the single "build everything" step used when a pipeline is collapsed.
-- @reason@ is a human-readable explanation of why we collapsed, shown in the
-- step label. The step has no @depends_on@: it is the only step.
bigStep :: Config -> String -> [(Text, FilePath)] -> Value
bigStep config reason jobs =
  object
    [ "label"   .= ("Build everything (" ++ reason ++ ")")
    , "command" .= bigStepCommand (configPostBuildHook config) jobs
    , "key"     .= ("nix-buildkite-build-all" :: String)
    ]

-- | The command for the collapsed step: a small prelude passing the job
-- derivations to 'buildAllScript' as positional arguments (and the post-build
-- hook, if any, via an environment variable), followed by the script itself.
--
-- Drv store paths are shell-safe (alphanumerics plus @/-._@) so they are
-- emitted bare. Note: 'buildAllScript' realises them in a single @nix-store@
-- invocation, which is bounded by @ARG_MAX@ (~2MB, i.e. tens of thousands of
-- drvs) — fine for realistic pipelines, and a small max-steps keeps the job set
-- well under that.
bigStepCommand :: Maybe FilePath -> [(Text, FilePath)] -> Value
bigStepCommand postBuildHook jobs = String (pack prelude <> buildAllScript)
  where
    drvPaths = map snd jobs

    hookLine = case postBuildHook of
      Nothing   -> ""
      Just path -> "export NBK_POST_BUILD_HOOK=" ++ shellSingleQuote path ++ "\n"

    -- Pass the derivations as positional arguments, one per line for legibility.
    setArgs = "set -- " ++ intercalate " \\\n  " drvPaths ++ "\n"

    prelude = hookLine ++ setArgs

-- | Single-quote a string for safe inclusion in a shell command.
shellSingleQuote :: String -> String
shellSingleQuote s = "'" ++ concatMap escape s ++ "'"
  where
    escape '\'' = "'\\''"
    escape c    = [c]

-- | Convert a derivation path to a valid Buildkite step key.
-- Buildkite step keys are limited to 100 characters and may only contain
-- alphanumeric characters, '/', and '-'.
stepify :: String -> String
stepify = take 99 . map replace . takeBaseName
  where
    replace x | isAlphaNum x = x
    replace '/' = '/'
    replace '-' = '-'
    replace _ = '_'


-- Internal helpers

withTime :: String -> IO a -> IO a
withTime label k = do
  before <- click
  res <- k
  after <- click
  let delta = diffTimeSpec before after
  hPutStrLn stderr $ printf "%s took %F seconds" label (fromIntegral (toNanoSecs delta) / (1_000_000_000 :: Double))
  pure res
  where
    click = getTime Monotonic

nixInstantiate :: String -> IO [String]
nixInstantiate jobsExpr = withTime "nix-instantiate" (Prelude.lines <$> readProcess "nix-instantiate" [ jobsExpr ] "")

nixBuildDryRun :: [String] -> IO [String]
nixBuildDryRun jobsExpr = withTime "nix-build --dry-run" $
  withCreateProcess ((proc "nix-build" (["--dry-run"] ++ jobsExpr)) { std_err = CreatePipe }) $ \ _stdin _stdout stderrHndl prchndl -> do
    inputLines <- Prelude.lines <$> case stderrHndl of
      Just hndl -> hGetContents hndl
      Nothing -> pure []
    -- See Note: [nix-build --dry-run output]
    let stripLeadingWhitespace = dropWhile (==' ')
    let theseLine line = List.isPrefixOf "these" line || List.isPrefixOf "this" line
    let buildLine line = theseLine line && List.isSubsequenceOf "built" line
    let fetchLine line = theseLine line && List.isSubsequenceOf "fetched" line

    -- dump the output to stderr
    mapM_ (hPutStrLn stderr) inputLines

    let res = map stripLeadingWhitespace . takeWhile (not . fetchLine) . drop 1 $ dropWhile (not . buildLine) inputLines
    exitCode <- waitForProcess prchndl
    case exitCode of
      ExitSuccess -> pure res
      ExitFailure err -> error $ "nix-build --dry run failed with exit code: " ++ show err

-- Note: [nix-build --dry-run output]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The output of `nix-build --dry-run` looks like this (on stderr! not stdout):
-- > trace: ...
-- > these 201 derivations will be built:
-- >   /nix/store/foo.drv
-- >   /nix/store/bar.drv
-- >   /nix/store/baz.drv
-- >   ...
-- > these 499 paths will be fetched (1607.21 MiB download, 6356.04 MiB unpacked):
-- >   /nix/store/foo1
-- >   ...
-- What we want to do is drop everything until the first line starting with "these", strip the leading whitespace,
-- and grab everything until we get to the second "these"
--
-- NB: you might not have any derivations to be built.
--
-- NB: if only one deriviation needs to be built, then you need to look out for "this" rather than "these"

add :: AdjacencyMap FilePath -> FilePath -> IO (AdjacencyMap FilePath)
add g drvPath =
  if hasVertex drvPath g then
    return g

  else
    fmap (parseOnly parseDerivation) (readFile drvPath) >>= \case
      Left _ ->
        return g

      Right Derivation{ inputDrvs } -> do
        deps <- foldr (\dep m -> m >>= \g' -> add g' dep) (pure g) (Map.keys inputDrvs)

        let g' = overlays (edge drvPath <$> Map.keys inputDrvs)

        return $ overlay deps g'

{- Note [Pipeline batching]

Buildkite has a limit of 500 jobs per pipeline upload. To support larger
pipelines, we split the steps into batches and output one JSON object per
line. The calling shell script uploads each batch sequentially.

We topologically sort the steps so that dependencies appear before the steps
that depend on them. This ensures that when uploading batches sequentially,
a step's dependencies are always uploaded in an earlier (or the same) batch.
Buildkite resolves `depends_on` references across separate uploads, so this
ordering is sufficient for correctness.
-}
