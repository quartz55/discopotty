{ lib, stdenv, ocamlPackages, ocaml }:

stdenv.mkDerivation {
  pname = "discopotty";
  version = "0.0.1";

  src = lib.filterGitSource {
    src = ./.;
    dirs = [ "bin" ];
    files = [ "dune" "dune-project" "discopotty.opam" ];
  };

  nativeBuildInputs = [
    ocamlPackages.dune
    ocaml
  ];
  buildInputs = with ocamlPackages; [
    disco
    angstrom
    containers
    toml
    relog
    eio_luv
  ];

  buildPhase = ''
    dune build bin/bot/discopotty.exe --display=short --profile=release
  '';
  installPhase = ''
    mkdir -p $out/bin
    mv _build/default/bin/bot/discopotty.exe $out/bin/discopotty
  '';
}
