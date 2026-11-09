{
  description = "orpheus.el development and demo environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
    in
    {
      devShells = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          fonts = pkgs.makeFontsConf {
            fontDirectories = [ pkgs.dejavu_fonts ];
          };
        in
        {
          default = pkgs.mkShell {
            packages = with pkgs; [
              bash
              coreutils
              emacs
              ffmpeg
              findutils
              xvfb-run
            ];
            FONTCONFIG_FILE = fonts;
          };
        });
    };
}
