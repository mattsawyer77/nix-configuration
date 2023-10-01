{ pkgs, ... }: {
  home.packages = [ pkgs.wezterm ];
  programs.wezterm = {
    enable = true;
    enableZshIntegration = true;
    extraConfig = (builtins.readFile ./wezterm.lua);
  };
}
