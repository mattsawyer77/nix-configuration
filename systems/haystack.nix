{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [
    ../modules/containerd
  ];

  system.stateVersion = "22.11";

  users.users.sawyer = {
    isNormalUser = true;
    home = "/home/sawyer";
    description = "Matt Sawyer";
    extraGroups = [
      "wheel"
      "networkmanager"
      "docker"
    ];
    shell = pkgs.zsh;
  };
  users.users.sawyer.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDUkwAz+dBcwzjI9Hwz0ETqH2GSOd3Og4cgisF8NC7Ck m.sawyer@KD21QWDKW7"
  ];

  networking = {
    networkmanager.enable = true;
    hostName = "haystack";
    nameservers = ["172.27.1.1" "1.0.0.1" "8.8.4.4"];
    firewall = {
      enable = true;
      allowPing = true;
      allowedTCPPorts = [
        22 # ssh
        2022 # et
        6443 # k3s
      ];
    };
  };

  i18n.defaultLocale = "en_US.UTF-8";

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
  programs.zsh.enable = true;

  services.eternal-terminal.enable = true;
  services.openssh.enable = true;

  time.timeZone = "America/Los_Angeles";

  virtualisation.docker.enable = true;
  virtualisation.docker.extraOptions = "--bip 192.168.10.1/24";
}
