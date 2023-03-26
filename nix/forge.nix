{ lib
, stdenv
, fetchFromGitHub
, cmake
, pkg-config
, expat
, fontconfig
, freeimage
, freetype
, boost
, mesa
, libGLU
, libGL
, glfw3
, glm
, SDL2
}:

stdenv.mkDerivation rec {
  pname = "forge";
  version = "1.0.8";

  src = fetchFromGitHub {
    owner = "arrayfire";
    repo = pname;
    rev = "v1.0.8";
    sha256 = "sha256-lSZAwcqAHiuZkpYcVfwvZCfNmEF3xGN9S/HuZQrGeKU=";
  };
  glad = fetchFromGitHub {
    owner = "arrayfire";
    repo = "glad";
    rev = "ef8c5508e72456b714820c98e034d9a55b970650";
    sha256 = "sha256-u9Vec7XLhE3xW9vzM7uuf+b18wZsh/VMtGbB6nMVlno=";
  };

  patches = [ ./forge-no-download.patch ];
  postPatch = ''
    mkdir -p ./extern
    cp -R --no-preserve=mode,ownership ${glad} ./extern/glad
  '';

  cmakeFlags = [ "-DAF_GLAD_PATH=extern/glad" ];

  doCheck = true;

  nativeBuildInputs = [
    cmake
    pkg-config
  ];

  buildInputs = [
    expat
    fontconfig
    freetype
    boost.out
    boost.dev
    freeimage
    mesa
    libGLU
    libGL
    glfw3
    glm
    SDL2
  ];

  meta = with lib; {
    description = "An OpenGL interop library that can be used with ArrayFire or any other application using CUDA or OpenCL compute backend";
    longDescription = ''
      An OpenGL interop library that can be used with ArrayFire or any other application using CUDA or OpenCL compute backend.
      The goal of Forge is to provide high performance OpenGL visualizations for C/C++ applications that use CUDA/OpenCL.
      Forge uses OpenGL >=3.3 forward compatible contexts, so please make sure you have capable hardware before trying it out.
    '';
    license = licenses.bsd3;
    homepage = "https://arrayfire.com/";
    platforms = platforms.linux;
    maintainers = with maintainers; [ chessai ];
  };
}
