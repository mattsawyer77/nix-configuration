{ config, pkgs, ... }:

{
  home.username = "matt";
  home.homeDirectory = "/Users/matt";
  home.packages = [ ];
  programs.home-manager.enable = true;
}
