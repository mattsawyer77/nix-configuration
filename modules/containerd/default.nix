{ pkgs, ... }:

with pkgs;
{
  environment.systemPackages = [ containerd ];
  virtualisation.containerd = {
    enable = true;
    settings = (builtins.readFile ./config.toml);
  };
}
