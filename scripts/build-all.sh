#!/usr/bin/env bash
# Body of the collapsed "build everything" step (see Note [Collapsing large
# pipelines] in src/NixBuildkite.hs, which embeds this file). Builds every .drv
# passed as an argument; NBK_POST_BUILD_HOOK, if set, is forwarded to nix-store.
set -uo pipefail

log="$(mktemp)"

# Buildkite runs steps under `bash -e`; turn that off so a build failure does
# not abort us before we print the summary below.
set +e

# `---` collapses this section in the Buildkite UI. We build with --keep-going,
# and drop nix's "N dependencies of derivation ... failed to build" cascade from
# the console — a few leaf failures produce hundreds of those lines. Everything
# else (progress, the real errors) still streams; the full log is kept in $log.
echo "--- :nix: Building everything"
if [ -n "${NBK_POST_BUILD_HOOK:-}" ]; then
  nix-store --post-build-hook "$NBK_POST_BUILD_HOOK" --keep-going -r "$@" 2>&1
else
  nix-store --keep-going -r "$@" 2>&1
fi | tee "$log" | grep --line-buffered -vE '^error: [0-9]+ dependencies of derivation '
status="${PIPESTATUS[0]}"

if [ "$status" -ne 0 ]; then
  # `+++` keeps this expanded in the UI. List the derivations that actually
  # failed, deduped; the store path names each one. nix reports these as
  # "Cannot build '<drv>'" / "builder for '<drv>' failed" lines.
  echo "+++ :x: Some builds failed"
  echo "Re-run the pipeline to get a granular per-job step for each of these:"
  echo
  grep -E "Cannot build '|builder for '.* failed" "$log" \
    | grep -oE "/nix/store/[^ ']+\.drv" | sort -u
fi

exit "$status"
