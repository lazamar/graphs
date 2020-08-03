{ nixpkgs ? <nixpkgs> }:
let
    pkgs = import nixpkgs {};

    # Commands
    commands = {
      plot = pkgs.writeShellScriptBin "plot" ''
        stack run --silent | dot -Tpng > image.png
        open image.png
        '';
    };
in
pkgs.stdenv.mkDerivation {
    name = "graph";
    # withHoogle = true;
    buildInputs =
      [
        pkgs.graphviz
        commands.plot
      ];
}
