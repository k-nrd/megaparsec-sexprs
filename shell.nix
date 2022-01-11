{ system ? builtins.currentSystem, compiler ? null }:
let
  pkgs = import ./nix { inherit system compiler; };
in
pkgs.mkShell {
  buildInputs = [
    pkgs.megaparsec-sexprs.shell
  ];
  shellHook = ''
    export LD_LIBRARY_PATH=${pkgs.megaparsec-sexprs.shell}/lib:$LD_LIBRARY_PATH
    logo
  '';
}
