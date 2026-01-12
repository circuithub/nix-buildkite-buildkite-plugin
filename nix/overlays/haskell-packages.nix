self:
super:

let
  inherit (self) haskell haskellPackages;

  inherit (haskellPackages) callCabal2nix;

  inherit (haskell.lib) appendConfigureFlag dontCheck packagesFromDirectory;

  inherit (super.lib) composeExtensions cleanSource;

  WError =
    drv: appendConfigureFlag drv "--ghc-option=-Werror";

  configurations =
    _self: _super: {
      # Tests require nix-instantiate to run, which needs access to /nix/var
      # and the nix daemon. These aren't available inside the nix sandbox,
      # so we disable tests here and run them separately in CI.
      nix-buildkite = dontCheck (WError (callCabal2nix "nix-buildkite" (cleanSource ../../.) {}));
    };

in
{
  haskellPackages =
    super.haskellPackages.override
      (
        old:
          {
            overrides =
              composeExtensions
                (old.overrides or (_: _: {}))
                (
                  composeExtensions
                    (packagesFromDirectory { directory = ./haskell-packages; })
                    configurations
                );
          }
      );
}
