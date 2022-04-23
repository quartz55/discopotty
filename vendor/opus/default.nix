{ 
  ocamlPackages
  , pkg-config
  , libopus
}:

with ocamlPackages;
buildDunePackage {
  pname = "opus";
  version = "1.3.1";
  src = ./.;
  propagatedBuildInputs = [
    pkg-config
    libopus
    ctypes
    dune-configurator
  ];
}