{ lib
, ocamlPackages
}:

with ocamlPackages;
buildDunePackage {
  pname = "disco";
  version = "0.0.1";

  src = lib.filterGitSource {
    src = ./.;
    dirs = [ "lib" "tests" ];
    files = [ "dune" "dune-project" "disco.opam" ];
  };

  propagatedBuildInputs = [
    disco-opus
    angstrom
    containers
    containers-data
    eio
    httpaf
    hxd
    ke
    mtime
    ppx_deriving
    ppx_yojson_conv
    ptime
    sodium
    stdint
    streaming
    tls
    toml
    uri
    websocketaf
    gluten
    yojson
    relog
  ];
}
