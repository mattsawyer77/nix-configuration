{ config
, pkgs
, needsFirewall ? false
, nameservers ? [ ]
, search ? [ ]
, ...
}:

{
  # enable the tailscale daemon; this will do a variety of tasks:
  # 1. create the TUN network device
  # 2. setup some IP routes to route through the TUN
  services.tailscale = { enable = true; };

  environment.systemPackages = [ pkgs.tailscale ];
}
// (if needsFirewall then
  {
    networking.firewall.allowedUDPPorts = [ 41641 ];
    networking.search = [ "example.ts.net" ];
  } else { })
// (if (builtins.isList nameservers && builtins.length nameservers > 0) then
  {
    networking.nameservers = nameservers;
  } else { })
  // (if (builtins.isList search && builtins.length search > 0) then
  {
    networking.search = search;
  } else { })
