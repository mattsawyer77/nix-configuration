{
  # configDir ? ".hammerspoon",
  ...
}: let
  configDir = ".hammerspoon";
  configFile = "${configDir}/init.lua";
in {
  home.file."hammerspoon-config" = {
    source = ./init.lua;
    target = configFile;
    force = true;
  };
}
