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
  # inputsFrom = [ self.packages.${stdenv.system}.discopotty ];
  buildInputs = with pkgs; [
    ocaml
    ocamlPackages.ocaml-lsp
    ocamlPackages.dune

    ocamlformat
    ocamlPackages.merlin
    ocamlPackages.utop
    ffmpeg
    youtube-dl
  ];
})
# .overrideAttrs (o: {
#   propagatedBuildInputs = filterDrvs o.propagatedBuildInputs;
#   buildInputs = filterDrvs o.buildInputs;
# })