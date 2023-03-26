{ boost
, cmake
, fetchFromGitHub
, forge
, freeimage
, fftw
, fftwFloat
, gtest
, lib
, libGLU
, libGL
, mesa
, ocl-icd
, openblas
, opencl-clhpp
, pkg-config
, python3
, span-lite
, spdlog
, stdenv
}:

stdenv.mkDerivation rec {
  pname = "arrayfire";
  version = "3.8.3";
  src = fetchFromGitHub {
    owner = pname;
    repo = pname;
    rev = "v${version}";
    sha256 = "sha256-Chk7koBv66JsfKV6+y6wg21snXYZswo6hjYm8rYEbbs=";
  };

  assets = fetchFromGitHub {
    owner = pname;
    repo = "assets";
    rev = "cd08d749611b324012555ad6f23fd76c5465bd6c";
    sha256 = "sha256-v4uhqPz1P1g1430FTmMp22xJS50bb5hZTeEX49GgMWg=";
  };
  clblast = fetchFromGitHub {
    owner = "cnugteren";
    repo = "CLBlast";
    rev = "4500a03440e2cc54998c0edab366babf5e504d67";
    sha256 = "sha256-I25ylQp6kHZx6Q7Ph5r3abWlQ6yeIHIDdS1eGCyArZ0=";
  };
  clfft = fetchFromGitHub {
    owner = pname;
    repo = "clfft";
    rev = "arrayfire-release";
    sha256 = "sha256-vJo1YfC2AJIbbRj/zTfcOUmi0Oj9v64NfA9MfK8ecoY=";
  };
  glad = fetchFromGitHub {
    owner = pname;
    repo = "glad";
    rev = "ef8c5508e72456b714820c98e034d9a55b970650";
    sha256 = "sha256-u9Vec7XLhE3xW9vzM7uuf+b18wZsh/VMtGbB6nMVlno=";
  };
  threads = fetchFromGitHub {
    owner = pname;
    repo = "threads";
    rev = "4d4a4f0384d1ac2f25b2c4fc1d57b9e25f4d6818";
    sha256 = "sha256-qqsT9woJDtQvzuV323OYXm68pExygYs/+zZNmg2sN34=";
  };
  test-data = fetchFromGitHub {
    owner = pname;
    repo = "arrayfire-data";
    rev = "a5f533d7b864a4d8f0dd7c9aaad5ff06018c4867";
    sha256 = "sha256-AWzhsrDXyZrQN2bd0Ng/XlE8v02x7QWTiFTyaAuRXSw=";
  };

  cmakeFlags = [
    "-DBUILD_TESTING=ON"
    "-DAF_BUILD_EXAMPLES=ON"
    "-DAF_BUILD_FORGE=OFF"
    "-DAF_USE_RELATIVE_TEST_DIR=ON"

    "-DAF_ASSETS_PATH=extern/assets"
    "-DAF_GLAD_PATH=extern/glad"
    "-DAF_THREADS_PATH=extern/threads"
  ] ++ (lib.optional stdenv.isDarwin "-DAF_BUILD_OPENCL=OFF");

  patches = [ ./arrayfire-no-download.patch ];
  postPatch = ''
    mkdir -p ./build/include/CL
    cp -R --no-preserve=mode,ownership ${opencl-clhpp}/include/CL/cl2.hpp ./build/include/CL/cl2.hpp

    cp -R --no-preserve=mode,ownership ${assets} ./extern/assets

    mkdir -p ./build/third_party/CLBlast/src
    cp -R --no-preserve=mode,ownership ${clblast} ./build/third_party/CLBlast/src/CLBlast-ext

    mkdir -p ./src/backend/opencl/extern
    cp -R --no-preserve=mode,ownership ${clfft} ./src/backend/opencl/extern/clfft

    cp -R --no-preserve=mode,ownership ${glad} ./extern/glad
    mkdir -p ./src/backend/cpu/extern
    cp -R --no-preserve=mode,ownership ${threads} ./src/backend/cpu/extern/threads
    cp -R --no-preserve=mode,ownership ${test-data} ./test/data

    substituteInPlace src/api/unified/symbol_manager.cpp \
      --replace '"/opt/arrayfire-3/lib/",' \
                "\"$out/lib/\", \"/opt/arrayfire-3/lib/\","
  '';

  # Some tests currently fail, see https://github.com/arrayfire/arrayfire/issues/3384
  doCheck = false;
  checkPhase = ''
    export LD_LIBRARY_PATH="${forge}/lib:${freeimage}/lib:$LD_LIBRARY_PATH"
    AF_TRACE=all AF_PRINT_ERRORS=1 ctest -v -j1
  '';

  buildInputs = [
    forge
    boost.out
    boost.dev
    fftw
    fftwFloat
    freeimage
    gtest
    libGLU
    libGL
    mesa
    (openblas.override { blas64 = false; })
    opencl-clhpp
    span-lite
    spdlog
  ] ++ (lib.optional stdenv.isLinux ocl-icd);

  nativeBuildInputs = [
    cmake
    pkg-config
    python3
  ];

  meta = with lib; {
    description = "A general-purpose library for parallel and massively-parallel computations";
    longDescription = ''
      A general-purpose library that simplifies the process of developing software that targets parallel and massively-parallel architectures including CPUs, GPUs, and other hardware acceleration devices.";
    '';
    license = licenses.bsd3;
    homepage = "https://arrayfire.com/";
    platforms = platforms.linux ++ platforms.darwin;
    maintainers = with maintainers; [ chessai ];
  };
}
