{ stdenv
, llvmPackages_14
, lib
, fetchFromGitHub
, cmake
, libpng
, libjpeg
, mesa
, eigen
, openblas
, blas
, lapack
}:

assert blas.implementation == "openblas" && lapack.implementation == "openblas";

stdenv.mkDerivation rec {
  pname = "halide";
  version = "15.0.0";

  src = fetchFromGitHub {
    owner = "halide";
    repo = "Halide";
    rev = "v${version}";
    sha256 = "sha256-te9Yn/rmA0YSulnxXL/y5d8PFphjQPgZUDWHNn7oqMg=";
  };

  cmakeFlags = [ "-DWARNINGS_AS_ERRORS=OFF" "-DWITH_PYTHON_BINDINGS=OFF" "-DTARGET_WEBASSEMBLY=OFF" ];

  doCheck = true;

  # Note: only openblas and not atlas part of this Nix expression
  # see pkgs/development/libraries/science/math/liblapack/3.5.0.nix
  # to get a hint howto setup atlas instead of openblas
  buildInputs = [
    llvmPackages_14.llvm
    llvmPackages_14.lld
    llvmPackages_14.openmp
    llvmPackages_14.libclang
    libpng
    libjpeg
    eigen
    openblas
  ]
  ++ lib.optional lib.isLinux mesa;

  nativeBuildInputs = [ cmake ];

  meta = with lib; {
    description = "C++ based language for image processing and computational photography";
    homepage = "https://halide-lang.org";
    license = licenses.mit;
    platforms = platforms.all;
    maintainers = with maintainers; [ ck3d atila ];
  };
}
