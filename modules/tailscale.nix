{ config, pkgs, needFirewall, ... }:

{
  # enable the tailscale daemon; this will do a variety of tasks:
  # 1. create the TUN network device
  # 2. setup some IP routes to route through the TUN
  services.tailscale = { enable = true; };
  environment.systemPackages = [ pkgs.tailscale ];
} // (if needFirewall then {
  # Let's open the UDP port with which the network is tunneled through
  networking.firewall.allowedUDPPorts = [ 41641 ];
} else { })
