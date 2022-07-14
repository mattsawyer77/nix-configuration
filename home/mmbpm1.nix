{ config, pkgs, ... }:

{
  home.homeDirectory = "/Users/matt";
  home.packages = [ ];
  home.stateVersion = "22.11";
  home.username = "matt";
  programs.home-manager.enable = true;
}
