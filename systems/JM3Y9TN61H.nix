{ config, pkgs, lib, ... }:
{
  system.primaryUser = "m.sawyer";
  system.stateVersion = 5;
  ids.gids.nixbld = 1000;
  ids.uids.nixbld = 1000;
  users.users."m.sawyer" = {
    name = "m.sawyer";
    home = "/Users/m.sawyer";
  };
  nix = {
    enable = true;
    gc = {
      automatic = true;
      interval = { Hour = 13; Minute = 0; };
      options = "--delete-older-than 2d";
    };
    extraOptions = ''
      system = aarch64-darwin
      extra-platforms = aarch64-darwin x86_64-darwin
      experimental-features = nix-command flakes
      build-users-group = nixbld
      trusted-users = root m.sawyer
      keep-outputs = true
      keep-derivations = true
    '';
  };
}
