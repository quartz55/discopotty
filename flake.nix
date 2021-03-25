{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
    nix-wrangle-pkg = { url = "github:timbertson/nix-wrangle"; flake = false; };
    opam2nix-pkg = { url = "github:timbertson/opam2nix"; flake = false; };
  };

  outputs = { self, nixpkgs, utils, nix-wrangle-pkg, opam2nix-pkg }:
    utils.lib.eachDefaultSystem (
      system:
        let
          pkgs = import nixpkgs { inherit system; };
          # nix-wrangle = import nix-wrangle-pkg { pkgs = pkgs; };
          # opam2nix = import opam2nix-pkg { inherit pkgs; inherit nix-wrangle; };
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
              nativeBuildInputs = with pkgs; [ pkg-config ];
              buildInputs = with pkgs; [
                esy
                ffmpeg
                youtube-dl
                opam
              ];
            };
          }
    );
}
