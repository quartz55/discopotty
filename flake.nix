{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
    ocaml-overlays = { url = "github:anmonteiro/nix-overlays"; flake = false; };
    nixpkgs-2021_03_20 = { url = "https://github.com/nixos/nixpkgs/archive/f5e8bdd07d1a.tar.gz"; flake = false; };
    dotsnix.url = "github:quartz55/dotsnix";
  };

  outputs = { self, nixpkgs, utils, ocaml-overlays, nixpkgs-2021_03_20, dotsnix }:
    utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = dotsnix.overlays;
        };
        anmonteiro = import nixpkgs {
          inherit system;
          overlays = [ (import ocaml-overlays) ];
        };
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
            babeltrace2
          ];
        };
      }
    );
}
