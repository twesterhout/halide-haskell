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
    arrayfire-haskell = {
      url = "github:twesterhout/arrayfire-haskell/withDevicePtr";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nix-filter.follows = "nix-filter";
    };
  };

  outputs = inputs@{ nixpkgs, flake-utils, nix-filter, ... }:
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
            (haskellPackages.callCabal2nix "halide-haskell" src {
              Halide = pkgs.halide;
              ffcall = with pkgs; symlinkJoin {
                inherit (libffcall) name;
                paths = [ libffcall.out libffcall.dev ];
              };
            }).overrideAttrs (attrs: {
              # pname = attrs.pname
              #   + lib.optionalString withIntelOpenCL "-intel-ocl"
              #   + lib.optionalString withCuda "-cuda";
              # name = "${pname}-${attrs.version}";
              # nativeBuildInputs = attrs.nativeBuildInputs
              #   ++ lib.optional withIntelOpenCL pkgs.makeWrapper;
              # propagatedBuildInputs = with pkgs;
              #   attrs.propagatedBuildInputs
              #   ++ lib.optionals withIntelOpenCL [ clinfo intel-ocl ocl-icd ];
              # ++ lib.optional withCuda nixGL.packages.${system}.nixGLDefault;
              # postInstall = (attrs.postInstall or "")
              #   + lib.optionalString withIntelOpenCL ''
              #   wrapProgram $out/bin/halide-haskell \
              #     --prefix LD_LIBRARY_PATH : ${pkgs.ocl-icd}/lib \
              #     --prefix OCL_ICD_VENDORS : ${pkgs.intel-ocl}/etc/OpenCL/vendors
              # ''
              #   + lib.optionalString withCuda ''
              #   prog="$out/bin/halide-haskell"
              #   hidden="$(dirname "$prog")/.$(basename "$prog")"-wrapped
              #   mv "$prog" "$hidden"
              #   echo "#!${pkgs.stdenv.shell}" > "$prog"
              #   chmod +x "$prog"
              # '';
              # We set withIntelOpenCL and withCuda such that dev shells can determine whether
              # they need extra dependencies
              # inherit withIntelOpenCL;
              # inherit withCuda;
            });
        in
        builder;

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
              halide-haskell = halide-haskell-for self hself;
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
                ] ++ lib.optional self.stdenv.isLinux halide-arrayfire;
              };
            });
        };
      };

      devShellFor = pkgs:
        let
          ps = pkgs.haskellPackages;
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
          ] ++ lib.optional pkgs.stdenv.isLinux halide-arrayfire;
          withHoogle = true;
          buildInputs = with pkgs; [ libffcall ocl-icd ];
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
            nixpkgs-fmt
            # Previewing markdown files
            python3Packages.grip
            # For debugging Halide
            clang_14
            # gcc
            # zlib
            # gdb
            clinfo
          ];
          # ++ lib.optional withCuda pkgs.nixgl.auto.nixGLDefault;
          shellHook = ''
            export PROMPT_COMMAND=""
            export PS1='(nix) GHC ${ps.ghc.version} \w $ '
            export LD_LIBRARY_PATH=${pkgs.libffcall.out}/lib:${pkgs.halide}/lib:${pkgs.ocl-icd}/lib:${pkgs.zlib}/lib:$LD_LIBRARY_PATH
          '';
          # + (if withIntelOpenCL then ''
          #   export LD_LIBRARY_PATH=$LD_LIBRARY_PATH
          #   export OCL_ICD_VENDORS="${pkgs.intel-ocl}/etc/OpenCL/vendors"
          # '' else "");
        };

      pkgsFor = system: args: import nixpkgs {
        inherit system;
        overlays = [
          inputs.arrayfire-haskell.overlays.default
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
