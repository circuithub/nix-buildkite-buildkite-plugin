# Five independent jobs with no inter-dependencies.
# Used to test that batching works for jobs without dependency ordering constraints.
let
  pkgs = import <nixpkgs> {};
  mkJob = name: pkgs.runCommand name {} "echo ${name} > $out";
in {
  job1 = mkJob "job1";
  job2 = mkJob "job2";
  job3 = mkJob "job3";
  job4 = mkJob "job4";
  job5 = mkJob "job5";
}
