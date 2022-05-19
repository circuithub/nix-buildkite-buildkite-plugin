# nix-buildkite-buildkite-plugin [![Build status](https://badge.buildkite.com/7918a1ba68d299f83ccc990292a97fa6eecd251703b2ca9427.svg)](https://buildkite.com/circuithub/nix-buildkite)

`nix-buildkite` is a Buildkite plugin that can take a Nix expression that
describes a set of builds and transforms them into separate Buildkite jobs.
`nix-buildkite` evaluates Nix to create derivations and then analyses these
derivations to find a topological ordering that will ensure steps have the
correct dependencies between them.

# Getting Started

## `jobs.nix`

First, create a `jobs.nix` file in your repository. This file will contain a
tree of all builds that you are interested in. We create this tree using nested
attrsets that eventually have leaves that are derivations.

For this example, we'll start by building the `nix-buildkite` project. Our
`jobs.nix` file is:

``` nix
let pkgs = import ./nix/pkgs {};
in
{
  nix-buildkite = pkgs.haskellPackages.nix-buildkite;
}
```

## `.buildkite/pipeline.yml`

Next, add a `.buildkite/pipeline.yml` file with the following contents:

``` yaml
steps:
  - command: nix-buildkite
    label: ":nixos: :buildkite:"
    plugins:
      circuithub/nix-buildkite:
        file: jobs.nix
```

## Add Your Pipeline

The final step is to add your pipeline to Buildkite. See
https://buildkite.com/docs/pipelines/defining-steps#getting-started for details
on how to do this. Once you have a pipeline created, make sure that the only
step declared in the pipeline configuration in Buildkite's UI is:

``` yaml
steps:
  - command: buildkite-agent pipeline upload
    label: ":pipeline:"
```

## Post-build hooks

The plugin accepts an optional `post-build-hook` argument, whose value
is the name (or path) of an executable that's compatible with Nix's
[post-build hook
semantics](https://nixos.org/manual/nix/stable/advanced-topics/post-build-hook.html):

``` yaml
steps:
  - command: nix-buildkite
    label: ":nixos: :buildkite:"
    plugins:
      circuithub/nix-buildkite:
        file: jobs.nix
        post-build-hook: /etc/nix/upload-to-cache.sh
```

When specified, the plugin will run this hook after its
`nix-instantiate` phase, and after each individual job that it
creates. This option is useful when you want to take advantage of
Nix's post-build hook feature (e.g., to upload the derivations created
by the pipeline), but you don't want to enable a system-wide
post-build hook. For example, you might only want to upload some
pipelines' outputs to your binary cache, or you might want to upload
different pipelines' outputs to different binary caches.

Note that in order to use this feature, you'll need to add the user
that the Buildkite agent runs as to `nix.trustedUsers`, as only
trusted users can run post-build hooks.

## Sit Back and Enjoy!

That's it! Following these steps should give you a working pipeline that builds
`nix-buildkite`.
