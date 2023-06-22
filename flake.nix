{
  description = "twesterhout/halide-haskell: Running Halide pipelines from Haskell";

  nixConfig = {
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
    # halide = {
    #   url = "github:halide/Halide";
    #   flake = false;
    # };
    arrayfire-nix = {
      url = "github:twesterhout/arrayfire-nix";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # inline-c = {
    #   url = "github:twesterhout/inline-c";
    #   flake = false;
    # };
    arrayfire-haskell = {
      url = "github:twesterhout/arrayfire-haskell/main";
      flake = false;
    };
    nixGL = {
      url = "github:guibou/nixGL";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, flake-utils, nix-filter, nixGL, ... }:
    let
      inherit (nixpkgs) lib;
      src = nix-filter.lib {
        root = ./.;
        include = [
          "src"
          "example"
          "test"
          "construction.png"
          "halide-haskell.cabal"
          "README.md"
          "test-readme/README.lhs"
          "LICENSE"
        ];
      };
      halide-haskell-for = pkgs: haskellPackages:
        let
          builder =
            { withIntelOpenCL
            , withCuda
            }:
            (haskellPackages.callCabal2nix "halide-haskell" src {
              Halide = pkgs.halide;
            }).overrideAttrs (attrs: rec {
              pname = attrs.pname
                + lib.optionalString withIntelOpenCL "-intel-ocl"
                + lib.optionalString withCuda "-cuda";
              name = "${pname}-${attrs.version}";
              nativeBuildInputs = attrs.nativeBuildInputs
                ++ lib.optional withIntelOpenCL pkgs.makeWrapper;
              propagatedBuildInputs = with pkgs;
                attrs.propagatedBuildInputs
                ++ lib.optionals withIntelOpenCL [ clinfo intel-ocl ocl-icd ]
                ++ lib.optional withCuda nixGL.packages.${system}.nixGLDefault;
              postInstall = (attrs.postInstall or "")
                + lib.optionalString withIntelOpenCL ''
                wrapProgram $out/bin/halide-haskell \
                  --prefix LD_LIBRARY_PATH : ${pkgs.ocl-icd}/lib \
                  --prefix OCL_ICD_VENDORS : ${pkgs.intel-ocl}/etc/OpenCL/vendors
              ''
                + lib.optionalString withCuda ''
                prog="$out/bin/halide-haskell"
                hidden="$(dirname "$prog")/.$(basename "$prog")"-wrapped
                mv "$prog" "$hidden"
                echo "#!${pkgs.stdenv.shell}" > "$prog"
                echo "exec ${pkgs.nixgl.auto.nixGLDefault}/bin/nixGL $hidden \"\$@\"" >> "$prog"
                chmod +x "$prog"
              '';
              # We set withIntelOpenCL and withCuda such that dev shells can determine whether
              # they need extra dependencies
              inherit withIntelOpenCL;
              inherit withCuda;
            });
        in
        lib.makeOverridable builder
          { withIntelOpenCL = false; withCuda = false; };

      with-markdown-unlit = hp: p: p.overrideAttrs (attrs: {
        nativeBuildInputs = (attrs.nativeBuildInputs or [ ]) ++ [ hp.markdown-unlit ];
      });

      overlayFor = args: self: super: {
        haskell = super.haskell // {
          packageOverrides = nixpkgs.lib.composeExtensions super.haskell.packageOverrides
            (hself: hsuper: rec {
              # arrayfire =
              #   (hself.callCabal2nix "arrayfire" inputs.arrayfire-haskell.outPath {
              #     af = pkgs.arrayfire;
              #   }).overrideAttrs (attrs: {
              #     configureFlags = (attrs.configureFlags or [ ]) ++ [ "-fdisable-default-paths" ];
              #   });
              halide-haskell = (halide-haskell-for self hself).override args;
              halide-JuicyPixels =
                (hself.callCabal2nix "halide-JuicyPixels" ./halide-JuicyPixels { });
              halide-arrayfire =
                (hself.callCabal2nix "halide-arrayfire" ./halide-arrayfire { });
              halide-readme = with-markdown-unlit hself
                (hself.callCabal2nix "halide-readme" ./test-readme { });
              halide-tutorial01 = with-markdown-unlit hself
                (hself.callCabal2nix "halide-tutorial01" ./tutorials/01-Basics { });
              halide-tutorial03 = with-markdown-unlit hself
                (hself.callCabal2nix "halide-tutorial03" ./tutorials/03-Inspecting { });
              halide-tutorial04 = with-markdown-unlit hself
                (hself.callCabal2nix "halide-tutorial04" ./tutorials/04-Debugging { });
              halide-tutorial05 = with-markdown-unlit hself
                (hself.callCabal2nix "halide-tutorial05" ./tutorials/05-Scheduling { });
              halide-all = self.buildEnv {
                name = "halide-all";
                paths = [
                  halide-haskell
                  halide-JuicyPixels
                  halide-readme
                  halide-tutorial01
                  halide-tutorial03
                  halide-tutorial04
                  halide-tutorial05
                ]; # ++ lib.optional self.stdenv.isLinux halide-arrayfire;
              };
            });
        };
      };

      devShellFor = pkgs:
        let
          ps = pkgs.haskellPackages;
          withIntelOpenCL = ps.halide-haskell.withIntelOpenCL;
          withCuda = ps.halide-haskell.withCuda;
        in
        ps.shellFor {
          packages = ps: with ps; [
            halide-haskell
            halide-JuicyPixels
            halide-readme
            halide-tutorial01
            halide-tutorial03
            halide-tutorial04
            halide-tutorial05
          ]; # ++ lib.optional pkgs.stdenv.isLinux halide-arrayfire;
          withHoogle = true;
          nativeBuildInputs = with pkgs; with ps; [
            # Building and testing
            cabal-install
            doctest
            markdown-unlit
            # Language servers
            haskell-language-server
            nil
            # Formatters
            fourmolu
            cabal-fmt
            nixpkgs-fmt
            # Previewing markdown files
            python3Packages.grip
            # For debugging Halide
            clang_14
            # gcc
            # zlib
            # gdb
          ]
          ++ lib.optional withIntelOpenCL clinfo
          ++ lib.optional withCuda pkgs.nixgl.auto.nixGLDefault;
          shellHook = ''
            export PROMPT_COMMAND=""
            export PS1='(nix) GHC ${ps.ghc.version} \w $ '
            export LD_LIBRARY_PATH=${pkgs.zlib}/lib:${pkgs.halide}/lib:$LD_LIBRARY_PATH
          '' + (if withIntelOpenCL then ''
            export LD_LIBRARY_PATH=${pkgs.ocl-icd}/lib:$LD_LIBRARY_PATH
            export OCL_ICD_VENDORS="${pkgs.intel-ocl}/etc/OpenCL/vendors"
          '' else "");
        };

      pkgsFor = system: args: import nixpkgs {
        inherit system;
        overlays = [
          nixGL.overlay
          (overlayFor args)
        ];
        config.allowUnfree =
          (args ? withIntelOpenCL) && args.withIntelOpenCL
          || (args ? withCuda) && args.withCuda;
      };
    in
    {
      packages = flake-utils.lib.eachDefaultSystemMap (system:
        with (pkgsFor system { }); {
          default = haskellPackages.halide-haskell;
          halide-haskell = haskellPackages.halide-haskell;
          halide-all = haskellPackages.halide-all;
          haskell = haskell.packages;
        });

      devShells = flake-utils.lib.eachDefaultSystemMap (system: {
        default = devShellFor (pkgsFor system { });
        cuda = devShellFor (pkgsFor system { withCuda = true; });
        intel-ocl = devShellFor (pkgsFor system { withIntelOpenCL = true; });
      });

      overlays = {
        default = overlayFor { };
        cuda = overlayFor { withCuda = true; };
        intel-ocl = overlayFor { withIntelOpenCL = true; };
      };
      templates.default = {
        path = ./template;
        description = "A minimal Haskell project using halide-haskell";
      };
    };
}
