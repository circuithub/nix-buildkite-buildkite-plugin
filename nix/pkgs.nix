import (builtins.fetchTarball {
  name = "nixos-26.05";
  url = "https://github.com/nixos/nixpkgs/archive/7fc6f2c20af09cdcaf48b92ec3121860139ec668.tar.gz";
  sha256 = "sha256-bNyvoIyOCu7lzoCpWKWGIsHpTtERUWzqYWFPlN++WTw=";
}) {
  overlays = import ./overlays.nix;
}
