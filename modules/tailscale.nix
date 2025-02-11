{ config
, pkgs
, networkInterfaceName
, needFirewall
, ... }:

{
  # enable the tailscale daemon; this will do a variety of tasks:
  # 1. create the TUN network device
  # 2. setup some IP routes to route through the TUN
  services.tailscale = {
    enable = true;
    useRoutingFeatures = "server";
    # extraSetFlags = [
    #   "--advertise-exit-node"
    # ];
  };
  environment.systemPackages = with pkgs; [
    ethtool
    tailscale
  ];
  systemd.services."tailscale-${networkInterfaceName}" = {
    description = "tailscale-${networkInterfaceName}";
    serviceConfig = {
      Type = "oneshot";
      User = "root";
      ExecStart = "${pkgs.ethtool}/bin/ethtool -K ${networkInterfaceName} rx-udp-gro-forwarding on rx-gro-list off";
    };
    wantedBy = [ "network-pre.target" ];
  };
} // (if needFirewall then {
  # Let's open the UDP port with which the network is tunneled through
  networking.firewall.allowedUDPPorts = [ 41641 ];
} else { })
