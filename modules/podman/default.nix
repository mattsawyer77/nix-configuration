{ pkgs, ... }:

with pkgs;
{
  environment.systemPackages = [ podman ];
  virtualisation.podman = {
    enable = true;
    dockerCompat = true;
    dockerSocket.enable = true;
  };
}
