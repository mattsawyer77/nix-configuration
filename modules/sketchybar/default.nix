{ pkgs
, ...
}:

with pkgs;
{
  environment.systemPackages = [
    sketchybar
    sketchybar-app-font
  ];
  services.sketchybar = {
    enable = true;
    config = ''
      sketchybar --bar height=36
      sketchybar --update
      echo "sketchybar configuration loaded.."
    '';
  };
}
