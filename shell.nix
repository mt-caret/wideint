with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "env";
  buildInputs = [
    python3Packages.pyopencl
    python3Packages.numpy  
    python3Packages.hypothesis
  ];
}
