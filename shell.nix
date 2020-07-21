{ nixpkgs ? <nixpkgs> }:
let
    pkgs = import nixpkgs {};

    # Commands
    commands = {
      plot = pkgs.writeShellScriptBin "plot" ''
        stack runghc Graph.hs | dot -Tpng > image.png
        open image.png
        '';

      plotdfs = pkgs.writeShellScriptBin "plotdfs" ''
        stack runghc GraphDFS.hs | dot -Tpng > image-dfs.png
        open image-dfs.png
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
        commands.plotdfs
      ];
}
