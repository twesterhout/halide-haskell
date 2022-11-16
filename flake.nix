{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils/master";
    flake-compat = {
      url = "github:edolstra/flake-compat/master";
      flake = false;
    };
  };

  description = "Running Halide pipelines from Haskell";

  outputs = { self, nixpkgs, flake-utils, ... }:
    let
      overlay = se: su: {
        haskellPackages = su.haskellPackages.override {
          overrides = hse: _hsu: {
            "halide-haskell" = hse.callCabal2nix "halide-haskell" self { Halide = su.halide; };
          };
        };
        # haskell-hello =
        #   se.haskell.lib.justStaticExecutables
        #     se.haskellPackages.haskell-hello;
      };
    in
    { inherit overlay; } // flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; overlays = [ overlay ]; };
      in
      {
        defaultPackage = pkgs.halide-haskell;
        devShell = pkgs.haskellPackages.shellFor {
          packages = p: [ p."halide-haskell" ];
          buildInputs = [
            # pkgs.haskellPackages.haskell-language-server
            pkgs.haskellPackages.cabal-install
            # pkgs.haskellPackages.ghcid
            pkgs.haskellPackages.ormolu
            # pkgs.haskellPackages.hlint
            pkgs.nixpkgs-fmt
            pkgs.bash
            pkgs.halide
          ];
          shellHook = ''
            export PROMPT_COMMAND=""
            export PS1='(nix) \w $ '
            export HALIDE_PATH=${pkgs.halide}
            export LD_LIBRARY_PATH=${pkgs.halide}/lib:$LD_LIBRARY_PATH
          '';
          withHoogle = false;
        };
      }
    );
}
