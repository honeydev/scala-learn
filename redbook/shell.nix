{ pkgs ? import <nixpkgs> {} }:
let
  stdenv = pkgs.llvmPackages_15.stdenv;
in rec {
  project = stdenv.mkDerivation {
    name = "sparkshow";

    buildInputs = [
      pkgs.sbt
      pkgs.jdk21
      pkgs.postgresql
      pkgs.coursier
#      pkgs.metals
    ];
  };
shellHook = ''
     export PATH="$PATH:/home/honey/.local/share/coursier/bin"
  '';

}
