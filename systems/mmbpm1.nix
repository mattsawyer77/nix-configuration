{ config, pkgs, lib, ... }:
{
  system.stateVersion = 5;
  users.users."matt" = {
    name = "matt";
    home = "/Users/matt";
  };
  nix = {
    package = pkgs.nixVersions.stable;
    extraOptions = ''
      system = aarch64-darwin
      extra-platforms = aarch64-darwin x86_64-darwin
      experimental-features = nix-command flakes
      build-users-group = nixbld
      trusted-users = root matt
      keep-outputs = true
      keep-derivations = true
    '';
  };
}
