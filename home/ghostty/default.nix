{ pkgs
, ghosttyPackage ? pkgs.ghostty
, ...
}: {
  home.packages = [ ghosttyPackage ];
  programs.ghostty = {
    enable = true;
    enableZshIntegration = true;
    settings = {
      theme = "Kanagawa Dragon";
      font-size = 21;
      font-family = "PragmataPro Liga";
      # keybind = [
      #   "ctrl+h=goto_split:left"
      #   "ctrl+l=goto_split:right"
      # ];
    };
  };
}
