{ pkgs, stdenv, lib, self }:

let
  discoPkgs = pkgs.recurseIntoAttrs self.packages.${stdenv.system};
  discoDrvs = lib.filterAttrs (_: value: lib.isDerivation value) discoPkgs;
  filterDrvs = inputs:
    lib.filter
      (drv:
        !(lib.hasAttr "pname" drv) ||
        drv.pname == null ||
        !(lib.any
          (name: name == drv.pname || name == drv.name)
          (lib.attrNames discoDrvs)))
      inputs;
in
(pkgs.mkShell {
  OCAMLRUNPARAM = "b";
  inputsFrom = lib.attrValues discoDrvs;
  buildInputs = with pkgs; [
    ocamlPackages.ocaml-lsp
    ocamlformat
    ocamlPackages.merlin
    ocamlPackages.utop
    ffmpeg
    youtube-dl
  ] ++ lib.optionals stdenv.isDarwin [ fswatch ];
  propagatedBuildInputs = with pkgs.ocamlPackages; [
    alcotest
    qcheck
    qcheck-alcotest
  ];
}).overrideAttrs (o: {
  propagatedBuildInputs = filterDrvs o.propagatedBuildInputs;
  buildInputs = filterDrvs o.buildInputs;
})
