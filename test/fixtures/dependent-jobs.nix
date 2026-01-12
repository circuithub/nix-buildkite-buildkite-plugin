# Jobs with dependencies to test topological sorting.
#
# Dependency graph:
#   jobA (no deps)
#   jobB -> jobA
#   jobC -> jobB (transitively depends on jobA)
#   jobD -> jobA
#   jobE (no deps, independent)
#
# Expected topological order: jobA and jobE first, then jobB and jobD, then jobC.
let
  pkgs = import <nixpkgs> {};
  mkJob = name: deps: pkgs.runCommand name {
    buildInputs = deps;
  } "echo ${name} > $out";
in rec {
  jobA = mkJob "jobA" [];
  jobB = mkJob "jobB" [jobA];
  jobC = mkJob "jobC" [jobB];
  jobD = mkJob "jobD" [jobA];
  jobE = mkJob "jobE" [];
}
