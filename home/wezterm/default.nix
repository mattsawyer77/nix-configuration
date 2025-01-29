{
  pkgs,
  weztermPackage ? pkgs.wezterm,
  ...
}: {
  home.packages = [ weztermPackage ];
  programs.wezterm = {
    enable = true;
    enableZshIntegration = true;
    extraConfig = (builtins.readFile ./wezterm.lua);
  };
}
