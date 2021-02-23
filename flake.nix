{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }:
    utils.lib.eachDefaultSystem (
      system:
        let
          pkgs = nixpkgs.legacyPackages."${system}";
          # some symlink/relative path shenanigans happening 🤷
          esy = pkgs.writeShellScriptBin "esy" "${pkgs.nodePackages.esy}/lib/node_modules/.bin/esy $@";
        in
          rec {
            packages = {
              discopotty = pkgs.callPackage ./. { inherit pkgs; };
              docker = pkgs.dockerTools.buildImage {
                name = "discopotty";
                config = {
                  Cmd = [ "${packages.discopotty}/bin/discopotty" ];
                };
              };
            };
            defaultPackage = packages.discopotty;
            apps = {
              discopotty = utils.lib.mkApp { drv = packages.discopotty; };
            };
            defaultApp = apps.discopotty;
            devShell = pkgs.mkShell {
              buildInputs = with pkgs; [
                pkg-config
                libsodium
                esy
                # ocamlformat
                # ocamlPackages.ocaml-lsp
                # ocamlPackages.dune_2
              ];
            };
          }
    );
}
