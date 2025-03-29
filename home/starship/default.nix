{ pkgs, ... }: {
  home.packages = with pkgs; [ starship ];
  programs.starship = {
    enable = true;
    settings = builtins.fromTOML (builtins.readFile ./starship.toml);
  };
}
