{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}

module Main ( main ) where

-- algebraic-graphs
import Algebra.Graph.AdjacencyMap ( AdjacencyMap, edge, empty, hasVertex, overlay, overlays, postSet, vertexSet)

-- aeson
import Data.Aeson ( Value(..), (.=), encode, object )

-- attoparsec
import Data.Attoparsec.Text ( parseOnly )

-- base
import Data.Char
import Data.Maybe ( fromMaybe, listToMaybe )
import Data.Traversable ( for )
import Data.List (partition)
import qualified Prelude
import Prelude hiding ( getContents, lines, readFile, words )
import System.Environment ( getArgs, lookupEnv )

-- bytestring
import qualified Data.ByteString.Lazy

-- containers
import Data.Containers.ListUtils ( nubOrd )
import qualified Data.Map as Map
import qualified Data.Set as S

-- filepath
import System.FilePath

-- nix-derivation
import Nix.Derivation

-- process
import System.Process hiding ( env )

-- text
import Data.Text ( Text, pack, unpack )
import Data.Text.IO ( readFile )


main :: IO ()
main = do
  jobsExpr <- fromMaybe "./jobs.nix" . listToMaybe <$> getArgs

  postBuildHook <- do
    cmd <- lookupEnv "POST_BUILD_HOOK"
    case cmd of
      Nothing -> return []
      Just path -> return $ [ "--post-build-hook", path ]

  -- Run nix-instantiate on the jobs expression to instantiate .drvs for all
  -- things that may need to be built.
  inputDrvPaths <- nubOrd . Prelude.lines <$> readProcess "nix-instantiate" [ jobsExpr ] ""

  -- Build an association list of a job name and the derivation that should be
  -- realised for that job.
  drvs <- for inputDrvPaths \drvPath -> do
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

  g <- foldr (\(_, drv) m -> m >>= \g -> add g drv) (pure empty) drvs

  let jobSet = S.fromList $ map snd drvs

  -- Calculate the dependency graph
  -- For each vertex, we calculate its direct dependencies from the job set.
  -- This is:
  -- - any direct dependencies that are in the job set (base case)
  -- - the transitive job set dependencies of non-job set dependencies (recursive case)
  let closureG =
        Map.fromList
          [(v, us) |
            v <- S.toList (vertexSet g),
            let nexts = S.toList $ postSet v g,
            let (ins, outs) = partition (flip S.member jobSet) nexts,
            let us = S.unions $ (S.fromList ins):(map (\i -> fromMaybe S.empty $ Map.lookup i closureG) outs)
          ]

  let steps = map (uncurry step) drvs
        where
          step :: Text -> FilePath -> Value
          step label drvPath =
            object
              [ "label" .= unpack label
              , "command" .= String (pack $ unwords $ [ "nix-store" ] <> postBuildHook <> [ "-r", drvPath ])
              , "key" .= stepify drvPath
              , "depends_on" .= dependencies
              ]
            where
              dependencies = map stepify $ maybe [] S.toList $ Map.lookup drvPath closureG

  Data.ByteString.Lazy.putStr $ encode $ object [ "steps" .= steps ]

stepify :: String -> String
stepify = take 99 . map replace . takeBaseName
  where
    replace x | isAlphaNum x = x
    replace '/' = '/'
    replace '-' = '-'
    replace _ = '_'


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
