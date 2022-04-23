{
  ocamlPackages
}:

with ocamlPackages;
buildDunePackage {
  pname = "disco";
  version = "0.0.1";
  src = ./.;

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