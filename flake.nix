{
  description = "twesterhout/halide-haskell: Running Halide pipelines from Haskell";

  # We have a patched Halide version, so we need cachix such that users don't
  # have to compile Halide locally
  nixConfig = {
    extra-experimental-features = "nix-command flakes";
    extra-substituters = "https://halide-haskell.cachix.org";
    extra-trusted-public-keys = "halide-haskell.cachix.org-1:cFPqtShCsH4aNjn2q4PHb39Omtd/FWRhrkTBcSrtNKQ=";
  };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      # don't look for a flake.nix file in this repository
      # this tells Nix to retrieve this input as just source code
      flake = false;
    };
    halide = {
      url = "github:halide/Halide";
      flake = false;
    };
  };

  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    with builtins;
    let
      inherit (inputs.nixpkgs) lib;
      pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [
          (self: super: {
            halide = super.halide.overrideAttrs (attrs: {
              version = "16.0.0";
              src = inputs.halide;
              cmakeFlags = attrs.cmakeFlags ++ [ "-DWITH_TESTS=OFF" "-DWITH_TUTORIALS=OFF" ];
            });
          })
        ];
      };
      removeDots = version: concatStringsSep "" (splitVersion version);

      # only consider source dirs and package.yaml as source to our Haskell package
      # this allows the project to rebuild only when source files change, not e.g. readme
      src = inputs.nix-filter.lib {
        root = ./.;
        include = [
          "src"
          "example"
          "test"
          "halide-haskell.cabal"
          "README.md"
          "CHANGELOG.md"
          "LICENSE"
        ];
      };

      # This allows us to build a Haskell package with any given GHC version.
      # It will also affects all dependent libraries.
      # overrides allows us to patch existing Haskell packages, or introduce new ones
      # see here for specifics: https://nixos.wiki/wiki/Overlays
      haskellPackagesOverride = ps:
        trace "GHC version ${ps.ghc.version}"
          ps.override
          {
            overrides = self: super: {
              halide-haskell = self.callCabal2nix "halide-haskell" src {
                Halide = pkgs.halide;
              };
            };
          };

      outputsFor =
        { haskellPackages
        , name
        , package ? ""
        , ...
        }:
        let ps = haskellPackagesOverride haskellPackages; in
        {
          packages.${name} = ps.${package} or ps;
          devShells.${name} = ps.shellFor {
            packages = ps: [ ps.halide-haskell ];
            withHoogle = true;
            nativeBuildInputs = with pkgs; with ps; [
              cabal-install
              fourmolu
              haskell-language-server
              nixpkgs-fmt
            ];
            shellHook = ''
              export PROMPT_COMMAND=""
              export PS1='(nix) GHC ${ghcVersion} \w $ '
              export HALIDE_PATH=${pkgs.halide}
              export LD_LIBRARY_PATH=$HALIDE_PATH/lib:$LD_LIBRARY_PATH
            '';
          };
          # The formatter to use for .nix files (but not .hs files)
          # Allows us to run `nix fmt` to reformat nix files.
          formatter = pkgs.nixpkgs-fmt;
        };
    in
    foldl' (acc: conf: lib.recursiveUpdate acc (outputsFor conf)) { }
      (lib.mapAttrsToList (name: haskellPackages: { inherit name haskellPackages; }) pkgs.haskell.packages ++ [{
        haskellPackages = pkgs.haskellPackages;
        name = "default";
        package = "halide-haskell";
      }])
  );
}
