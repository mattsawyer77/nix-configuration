{
  config,
  pkgs,
  lib,
  ...
}: {
  system.stateVersion = "22.11";

  users.users.sawyer = {
    isNormalUser = true;
    home = "/home/sawyer";
    description = "Matt Sawyer";
    extraGroups = ["wheel" "networkmanager" "docker"];
    shell = pkgs.zsh;
  };
  users.users.sawyer.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIO1g1AytlaSn6IgGptJI41eQ66yi4hXYMLNRk3GBxWVE m.sawyer@KD21QWDKW7"
  ];

  networking.hostName = "sawyer-dev";
  networking.firewall = {
    enable = true;
    allowPing = true;
    allowedTCPPorts = [22 2022];
  };

  nix = {
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };
    settings = {
      allowed-users = ["sawyer" "@wheel"];
      trusted-users = ["sawyer" "@wheel"];
    };
  };

  programs.ssh = {
    startAgent = true;
    agentTimeout = "1h";
  };
  programs.tmux.enable = true;

  services.eternal-terminal.enable = true;
  services.openssh.enable = true;

  time.timeZone = "America/Los_Angeles";

  virtualisation.docker.enable = true;
}
