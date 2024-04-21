let
  pkgs = import (fetchTarball ("channel:nixpkgs-unstable")) { };
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    ocamlformat
    opam
  ];
  shellHook = ''
    eval $(opam env)
  '';
}
