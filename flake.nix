{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
    ocaml-overlays.url = "github:anmonteiro/nix-overlays";
    dotsnix.url = "github:quartz55/dotsnix";
  };

  outputs = { self, nixpkgs, utils, ocaml-overlays, dotsnix }:
    utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
          overlays = [
            ocaml-overlays.overlay
            (self: super: { ocamlPackages = super.ocaml-ng.ocamlPackages_5_00; })
            (self: super: {
              ocamlPackages = super.ocamlPackages // {
                ppx_yojson_conv = super.ocamlPackages.buildDunePackage rec {
                  pname = "ppx_yojson_conv";
                  version = "0.15.0";

                  src = builtins.fetchurl {
                    url = "https://github.com/janestreet/ppx_yojson_conv/archive/refs/tags/v${version}.tar.gz";
                    sha256 = sha256:0lyhvw73cq2sw1bwackmxk1bricw9y5p03g2qvjqphaaq9fi74lj;
                  };

                  propagatedBuildInputs = with self.ocamlPackages; [
                    base
                    ppx_js_style
                    ppx_yojson_conv_lib
                    ppxlib
                  ];
                };
                streaming = super.ocamlPackages.buildDunePackage rec {
                  pname = "streaming";
                  version = "master";
                  rev = "74a48634f81eefc5ff98103016b4be9645c1ea01";
                  src = builtins.fetchurl {
                    url = "https://github.com/odis-labs/streaming/archive/${rev}.tar.gz";
                    sha256 = sha256:0ckc1k3waq1ax873nmzqs67cnd15di7yskfba5vddzncc6jsn6rv;
                  };
                  propagatedBuildInputs = with self.ocamlPackages; [ stdlib-shims ];
                };
                stdint = super.ocamlPackages.stdint.overrideAttrs (_: rec {
                  rev = "cb95ca6dff6bd58aa555b872a5db6558837d52db";
                  src = builtins.fetchurl {
                    url = "https://github.com/kit-ty-kate/ocaml-stdint/archive/${rev}.tar.gz";
                    sha256 = sha256:0ib82j20zjq5111rwhl2281lbccbsi2cqibvbqcanim5ffd008l1;
                  };
                  patches = [ ];
                });
                eio = super.ocamlPackages.buildDunePackage {
                  pname = "eio";
                  version = "0.2";
                  src = builtins.fetchurl {
                    url = "https://github.com/ocaml-multicore/eio/archive/refs/tags/v0.2.tar.gz";
                    sha256 = sha256:047qsiajrcglnws1fmcbikkfgm7prl05rvf1ylfbd69vdxd9ykjm;
                  };

                  propagatedBuildInputs = with self.ocamlPackages; [
                    cstruct
                    lwt-dllist
                    optint
                    psq
                    fmt
                    mtime
                  ];
                };
                eio_luv = super.ocamlPackages.buildDunePackage {
                  pname = "eio_luv";
                  inherit (self.ocamlPackages.eio) version src;

                  propagatedBuildInputs = with self.ocamlPackages; [
                    eio
                    luv
                    luv_unix
                    logs
                    fmt
                  ];
                };
                luv_unix = super.ocamlPackages.buildDunePackage {
                  pname = "luv_unix";
                  inherit (self.ocamlPackages.luv) version src;
                  propagatedBuildInputs = with self.ocamlPackages; [
                    luv
                    ctypes
                    result
                  ];
                };
                # TODO @quartz55: upstream this
                websocketaf = super.ocamlPackages.websocketaf.overrideAttrs (_: rec {
                  rev = "49bb812266608835b56ea75bf18f75666639a760";
                  src = builtins.fetchurl {
                    url = "https://github.com/quartz55/websocketaf/archive/${rev}.tar.gz";
                    sha256 = sha256:0lsnrl8h5bmx866lflnbca4yhwzq4q67d15q51157khnchjr34rq;
                  };
                });

                # TODO @quartz55: update it upstream
                relog = super.ocamlPackages.buildDunePackage rec {
                  pname = "relog";
                  version = "master";
                  rev = "15a27a87dc15b1a6092f2197effd51577ca0eaf7";
                  src = builtins.fetchurl {
                    url = "https://github.com/quartz55/relog-native/archive/${rev}.tar.gz";
                    sha256 = sha256:01j75d8h22aj26wh7hw541gxdzmy7pb78dncyvxd52nv1xrc0fvf;
                  };
                  buildInputs = with self.ocamlPackages; [ reason ];
                  propagatedBuildInputs = with self.ocamlPackages; [
                    containers
                    ptime
                    yojson
                  ];
                  postPatch = ''
                    substituteInPlace lib/Formatter.re --replace "Bi_outbuf" "Buffer"
                  '';
                };
                disco-opus = super.callPackage ./vendor/opus { };
                disco = super.callPackage ./. { };
              };
            })
          ]
          ++ dotsnix.overlays;
        };
      in
      rec {
        packages = {
          disco = pkgs.ocamlPackages.disco;
          discopotty = pkgs.callPackage ./discopotty.nix { };
          # docker = pkgs.dockerTools.buildImage {
          #   name = "discopotty";
          #   config = {
          #     Cmd = [ "${packages.discopotty}/bin/discopotty" ];
          #   };
          # };
        };
        defaultPackage = packages.discopotty;
        apps = {
          discopotty = utils.lib.mkApp { drv = packages.discopotty; };
        };
        defaultApp = apps.discopotty;
        devShell = pkgs.callPackage ./shell.nix { inherit self; };
      }
    );
}
