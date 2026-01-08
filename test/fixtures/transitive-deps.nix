# Jobs with transitive dependencies through non-job intermediates.
#
# This tests the transitive closure calculation: when a job depends on
# something that isn't a job, we need to find the job dependencies of
# that intermediate derivation.
#
# Dependency graph:
#   jobA (no deps)
#   intermediate -> jobA (NOT in job set)
#   jobB -> intermediate (should transitively depend on jobA)
#
# Expected: jobB's depends_on should include jobA, even though
# jobB doesn't directly depend on jobA.
let
  pkgs = import <nixpkgs> {};
  mkDrv = name: deps: pkgs.runCommand name {
    buildInputs = deps;
  } "echo ${name} > $out";

  # This intermediate is NOT exported, so it won't be in the job set
  intermediate = mkDrv "intermediate" [jobA];

  jobA = mkDrv "jobA" [];
  jobB = mkDrv "jobB" [intermediate];
in {
  inherit jobA jobB;
  # Note: intermediate is NOT exported
}
