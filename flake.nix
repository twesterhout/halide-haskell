{
  description = "twesterhout/halide-haskell: Running Halide pipelines from Haskell";

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
    # halide = {
    #   url = "github:halide/Halide";
    #   flake = false;
    # };
    inline-c = {
      url = "github:twesterhout/inline-c";
      flake = false;
    };
    nixGL = {
      url = "github:guibou/nixGL";
      inputs.flake-utils.follows = "flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    with builtins;
    let
      inherit (inputs.nixpkgs) lib;
      pkgs = import inputs.nixpkgs { inherit system; };

      # Only consider source dirs and .cabal files as the source to our Haskell package.
      # This allows the project to rebuild only when the source files change.
      src = inputs.nix-filter.lib {
        root = ./.;
        include = [
          "src"
          "example"
          "test"
          "halide-haskell.cabal"
          "README.md"
          "test-readme/README.lhs"
          "LICENSE"
        ];
      };

      testWriteExpr = pkgs.stdenv.mkDerivation {
        pname = "testWriteExpr";
        version = "0.0.1";
        src = ./test;
        dontConfigure = true;
        buildPhase = ''
          clang++ -std=c++17 write_to_ostream.cpp -lHalide
        '';
        checkPhase = ''
          ./a.out
        '';
        doCheck = true;
        installPhase = ''
          mkdir -p $out/bin
          install a.out $out/bin/
        '';
        buildInputs = with pkgs; [ halide ];
        nativeBuildInputs = with pkgs; [ clang_14 ];
      };

      # checkedHalide = pkgs.halide.overrideAttrs (attrs: {
      #   doCheck = true;
      #   cmakeFlags = (attrs.cmakeFlags or [ ]) ++ [ "-DHalide_ENABLE_EXCEPTIONS=OFF" ];
      #   patchPhase = (attrs.patchPhase or "") + ''
      #     substituteInPlace test/correctness/CMakeLists.txt \
      #       --replace 'exception.cpp' '# exception.cpp'
      #   '';
      # });

      halide-haskell-for = haskellPackages:
        let
          builder =
            { withIntelOpenCL
            , withCuda
            , Halide
            }:
            (haskellPackages.callCabal2nix "halide-haskell" src { inherit Halide; }).overrideAttrs (attrs: rec {
              pname = attrs.pname
                + lib.optionalString withIntelOpenCL "-intel-ocl"
                + lib.optionalString withCuda "-cuda";
              name = "${pname}-${attrs.version}";
              nativeBuildInputs = attrs.nativeBuildInputs
                ++ lib.optional withIntelOpenCL pkgs.makeWrapper;
              propagatedBuildInputs = with pkgs;
                attrs.propagatedBuildInputs
                ++ lib.optionals withIntelOpenCL [ clinfo intel-ocl ocl-icd ]
                ++ lib.optional withCuda inputs.nixGL.packages.${system}.nixGLDefault;
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
                echo "exec ${inputs.nixGL.packages.${system}.nixGLDefault}/bin/nixGL $hidden \"\$@\"" >> "$prog"
                chmod +x "$prog"
              '';

              # We set withIntelOpenCL and withCuda such that dev shells can determine whether
              # they need extra dependencies
              inherit withIntelOpenCL;
              inherit withCuda;
            });
        in
        lib.makeOverridable builder
          { withIntelOpenCL = false; withCuda = false; Halide = pkgs.halide; };

      with-markdown-unlit = hp: p: p.overrideAttrs (attrs: {
        nativeBuildInputs = (attrs.nativeBuildInputs or [ ]) ++ [ hp.markdown-unlit ];
      });

      # This allows us to build a Haskell package with any given GHC version.
      # It will also affects all dependent libraries.
      # overrides allows us to patch existing Haskell packages, or introduce new ones
      # see here for specifics: https://nixos.wiki/wiki/Overlays
      haskellPackagesOverride = ps: args:
        ps.override
          {
            overrides = self: super: rec {
              inline-c-cpp =
                (self.callCabal2nix "inline-c-cpp" "${inputs.inline-c.outPath}/inline-c-cpp" { });
              halide-haskell = (halide-haskell-for self).override args;
              halide-JuicyPixels =
                (self.callCabal2nix "halide-JuicyPixels" ./halide-JuicyPixels { });
              halide-readme = with-markdown-unlit self
                (self.callCabal2nix "halide-readme" ./test-readme { });
              halide-tutorial01 = with-markdown-unlit self
                (self.callCabal2nix "halide-tutorial01" ./tutorials/01-Basics { });
              halide-tutorial03 = with-markdown-unlit self
                (self.callCabal2nix "halide-tutorial03" ./tutorials/03-Inspecting { });
              halide-tutorial04 = with-markdown-unlit self
                (self.callCabal2nix "halide-tutorial04" ./tutorials/04-Debugging { });
              halide-tutorial05 = with-markdown-unlit self
                (self.callCabal2nix "halide-tutorial05" ./tutorials/05-Scheduling { });
              halide-all = pkgs.buildEnv {
                name = "halide-all";
                paths = [
                  halide-haskell
                  halide-JuicyPixels
                  halide-readme
                  halide-tutorial01
                  halide-tutorial03
                  halide-tutorial04
                  halide-tutorial05
                ];
              };
            };
          };

      outputsFor =
        { haskellPackages
        , name
        , package ? ""
        , ...
        }:
        let
          ps = haskellPackagesOverride haskellPackages { };
          ps-cuda = haskellPackagesOverride haskellPackages { withCuda = true; };
          ps-intel-ocl = haskellPackagesOverride haskellPackages { withIntelOpenCL = true; };
        in
        {
          packages = {
            "${name}" = ps.${package} or ps;
            "${name}-cuda" = ps-cuda.${package} or ps-cuda;
            "${name}-intel-ocl" = ps-intel-ocl.${package} or ps-intel-ocl;
            testWriteExpr = testWriteExpr;
          };
          devShells =
            let
              genericShell = ps:
                let
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
                  ];
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
                  ++ lib.optional withCuda inputs.nixGL.packages.${system}.nixGLDefault;
                  shellHook = ''
                    export PROMPT_COMMAND=""
                    export PS1='(nix) GHC ${haskellPackages.ghc.version} \w $ '
                    export LD_LIBRARY_PATH=${pkgs.zlib}/lib:${pkgs.halide}/lib:$LD_LIBRARY_PATH
                    export CC=${pkgs.clang_14}/bin/clang
                    export CXX=${pkgs.clang_14}/bin/clang++
                  '' + (if withIntelOpenCL then ''
                    export LD_LIBRARY_PATH=${pkgs.ocl-icd}/lib:$LD_LIBRARY_PATH
                    export OCL_ICD_VENDORS="${pkgs.intel-ocl}/etc/OpenCL/vendors"
                  '' else "");
                };
            in
            {
              "${name}" = genericShell ps;
              "${name}-cuda" = genericShell ps-cuda;
              "${name}-intel-ocl" = genericShell ps-intel-ocl;
            };
          # The formatter to use for .nix files (but not .hs files)
          # Allows us to run `nix fmt` to reformat nix files.
          formatter = pkgs.nixpkgs-fmt;
        };
    in
    foldl' (acc: conf: lib.recursiveUpdate acc (outputsFor conf)) { }
      (lib.mapAttrsToList (name: haskellPackages: { inherit name haskellPackages; })
        (lib.filterAttrs (_: ps: ps ? ghc) pkgs.haskell.packages) ++ [
        {
          haskellPackages = pkgs.haskellPackages;
          name = "defaultGhc";
        }
        {
          haskellPackages = pkgs.haskellPackages;
          name = "default";
          package = "halide-haskell";
        }
      ])
  );
}
