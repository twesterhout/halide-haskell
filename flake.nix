{
  description = "twesterhout/halide-haskell: Running Halide pipelines from Haskell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-22.11";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      # don't look for a flake.nix file in this repository
      # this tells Nix to retrieve this input as just source code
      flake = false;
    };
  };

  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      # this style makes it easy to override non-Haskell packages, e.g. to patch them
      # pkgs = import inputs.nixpkgs { inherit system; overlays = []; };
      pkgs = inputs.nixpkgs.legacyPackages.${system};

      # only consider source dirs and package.yaml as source to our Haskell package
      # this allows the project to rebuild only when source files change, not e.g. readme
      src = inputs.nix-filter.lib {
        root = ./.;
        include = [
          "src"
          "cbits"
          "test"
          "cabal.project"
          "cabal.project.local"
          "halide-haskell.cabal"
        ];
      };

      # This allows us to build a Haskell package with any given GHC version.
      # It will also affects all dependent libraries.
      # overrides allows us to patch existing Haskell packages, or introduce new ones
      # see here for specifics: https://nixos.wiki/wiki/Overlays
      haskellPackagesFor =
        { ghcVersion
        , nixpkgs ? pkgs
        , haskellPackages ? nixpkgs.haskell.packages."ghc${ghcVersion}"
        }:
        haskellPackages.override {
          overrides = self: super: rec {
            halide = nixpkgs.halide;
            halide-haskell = self.callCabal2nix "halide-haskell" ./. { Halide = halide; };
          };
        };

      # A list of GHC versions and corresponding package overrides to use with `haskellPackagesFor`.
      configurations = [
        {
          ghcVersion = "90";
          nixpkgs = inputs.nixpkgs-stable.legacyPackages.${system};
        }
        { ghcVersion = "92"; }
        { ghcVersion = "94"; }
      ];


      # A utility function that creates a set containing key-value pairs constructed for each
      # element in `configurations`.
      foldConfigurations = f:
        builtins.foldl'
          (acc: conf:
            acc // { "ghc${conf.ghcVersion}" = f (haskellPackagesFor conf); }
          )
          { }
          configurations;

      # The version of GHC used for default package and development shell.
      defaultGhcVersion = "ghc92";
    in
    rec {
      packages = {
        # Build ising-glass-annealer for one given GHC versions.
        ising-glass-annealer = foldConfigurations (haskellPackages: haskellPackages.halide-haskell);
        default = packages.ising-glass-annealer.${defaultGhcVersion};
      };

      # Prepare a development shell for many diffent GHC versions.
      devShells = foldConfigurations
        (haskellPackages:
          haskellPackages.shellFor {
            packages = ps: [ ps.halide-haskell ];
            nativeBuildInputs = with haskellPackages; [
              cabal-install
              ormolu
              haskell-language-server
            ];
            shellHook = ''
              export PROMPT_COMMAND=""
              export PS1='(nix) GHC ${haskellPackages.ghc.version} \w $ '
              export HALIDE_PATH=${haskellPackages.halide}
              export LD_LIBRARY_PATH=$HALIDE_PATH/lib:$LD_LIBRARY_PATH
            '';
          }
        ) // {
        default = devShells.${defaultGhcVersion};
      };

      # The formatter to use for .nix files (but not .hs files)
      # Allows us to run `nix fmt` to reformat nix files.
      formatter = pkgs.nixpkgs-fmt;
    }
  );
}