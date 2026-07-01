#!/usr/bin/env bash
# Build every derivation passed as a positional argument, in a single
# nix-store invocation. This is the body of the "build everything" step that
# nix-buildkite emits when a pipeline exceeds its max-steps limit and is
# collapsed into one job (see Note [Collapsing large pipelines] in
# src/NixBuildkite.hs, which embeds this file at compile time via Template
# Haskell and prepends the arguments).
#
# Inputs:
#   $@                    the .drv paths to realise
#   NBK_POST_BUILD_HOOK   optional; passed to nix-store as --post-build-hook
#
# We build with --keep-going so one failure does not abort the rest. nix's
# output is hard to read when that happens: a few real failures near the leaves
# produce hundreds of "N dependencies of derivation ... failed to build"
# cascade lines that bury the actual errors. So we fold the full build output
# into a collapsed Buildkite log group and, on failure, reprint just the root
# failures in an expanded group at the end.
set -uo pipefail

log="$(mktemp)"

# A collapsed group (---): the full, noisy build output, hidden by default.
echo "--- :nix: Building everything"
if [ -n "${NBK_POST_BUILD_HOOK:-}" ]; then
  nix-store --post-build-hook "$NBK_POST_BUILD_HOOK" --keep-going -r "$@" 2>&1 | tee "$log"
else
  nix-store --keep-going -r "$@" 2>&1 | tee "$log"
fi
status="${PIPESTATUS[0]}"

if [ "$status" -ne 0 ]; then
  # An expanded group (+++): the derivations that actually failed. nix reports
  # these as "Cannot build '<drv>'" / "builder for '<drv>' failed" lines (a
  # remote failure emits both phrasings), while the far more numerous
  # "N dependencies of derivation ..." lines are just their cascade. We pull the
  # .drv path out of the failure lines and dedupe, so each failure is listed
  # once — the store path names the derivation (e.g. ...-check-formatting.drv).
  failed="$(grep -E "Cannot build '|builder for '.*' failed" "$log" \
    | grep -oE "/nix/store/[^ ']+\.drv" | sort -u)"

  echo "+++ :x: Some builds failed"
  echo "Re-run the pipeline to get a granular per-job step for each of these:"
  echo
  if [ -n "$failed" ]; then
    printf '%s\n' "$failed"
  else
    echo "(couldn't identify specific failures; see the collapsed build log above)"
  fi
fi

exit "$status"
