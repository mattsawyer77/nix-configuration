{ pkgs, ... }:

with pkgs;
{
  environment.systemPackages = [ podman ];
  virtualisation.podman = {
    enable = true;
    dockerCompat = true;
    dockerSocket.enable = true;
  };
  virtualisation.containers.registries.insecure = [
    # requires podman registry pod to be started -- not yet automated here
    "localhost:5001"
  ];
}
