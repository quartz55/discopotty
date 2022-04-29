{ lib
, fetchgit
, ocamlPackages
}:

with ocamlPackages;
let
  mirage-trace-viewer = buildDunePackage rec {
    pname = "mirage-trace-viewer";
    version = "master";

    useDune2 = true;

    minimumOCamlVersion = "4.08";

    src = fetchgit {
      url = "https://github.com/quartz55/mirage-trace-viewer";
      rev = "13b53d6";
      sha256 = "sha256-Yw84KPTruhs7REPFhRRhfy507xzg7Uoo3Wiyp+mSOq0=";
    };

    nativeBuildInputs = [ ];
    propagatedBuildInputs = [ ocplib-endian itv-tree cmdliner ];

    strictDeps = true;

    doCheck = true;
  };
in
buildDunePackage {
  pname = "mirage-trace-viewer-gtk";
  inherit (mirage-trace-viewer) version useDune2 src;

  buildInputs = [ mirage-trace-viewer ];

  propagatedBuildInputs = [
    lwt
    lablgtk
    cairo2-gtk
    cairo2
  ];
}
