let pkgs = import ./nix/pkgs.nix;
in (pkgs.haskell.lib.doCheck pkgs.haskellPackages.nix-buildkite).env.overrideAttrs (old: {
  nativeBuildInputs = old.nativeBuildInputs ++ [ pkgs.cabal-install ];
})
