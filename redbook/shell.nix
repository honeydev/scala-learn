{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  # Add other build inputs/dependencies here, for example:
  buildInputs = with pkgs; [
      pkgs.sbt
      pkgs.jdk21
      pkgs.postgresql
      pkgs.coursier
  ];

  # Optional: a script to run after entering the shell
  shellHook = ''
    export PATH="$PATH:/home/honey/.local/share/coursier/bin"
    echo "Welcome to the stdenv development shell!"
  '';
}

