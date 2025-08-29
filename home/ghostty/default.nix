{ pkgs
# , ghosttyPackage ? pkgs.ghostty
  , configDir
, ...
}: {
  # XXX: ghostty marked as broken on macOS as of 2025-08-11
  # home.packages = [ ghosttyPackage ];
  # programs.ghostty = {
  #   enable = true;
  #   enableZshIntegration = true;
  #   settings = {
  #     font-size = 21;
  #     font-family = "PragmataPro Liga";
  #     # theme = "dark:Kanagawa Wave,light:ayu_light"
  #     # theme = "dark:Ghostty Default StyleDark,light:ayu_light"
  #     # theme = "dark:Argonaut,light:ayu_light"
  #     theme = "dark:kanagawabones,light:ayu_light";
  #     window-padding-x = "10,10";
  #     window-padding-y = "10,10";
  #     adjust-cell-height = "15%";
  #     adjust-font-baseline = 2;
  #     cursor-style = "block";
  #     # keybind = [
  #     #   "ctrl+h=goto_split:left"
  #     #   "ctrl+l=goto_split:right"
  #     # ];
  #   };
  # };

  # for now, just copy the ghostty config file into place
  home.file."ghostty-config" = {
    source = ./config;
    target = "${configDir}/config";
    force = true;
  };
}
