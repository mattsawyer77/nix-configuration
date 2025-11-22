{ config, lib, pkgs, username, ... }:

let
  homeDirectory = "/home/" + username;
  doomDirectory = ".doom.d";
  goPathSuffix = "gocode";
  localBinPath = ".local/bin";
  npmPackagePath = ".config/npm-packages";
  # to update/regenerate, run node2nix -i <(echo '["bash-language-server", "prettier"]') --nodejs-18
  # then copy the resulting files into ./npm-packages
  npmPackages = import ./npm-packages { inherit pkgs; };
  homePackages = with pkgs; [
    (google-cloud-sdk.withExtraComponents [ google-cloud-sdk.components.gke-gcloud-auth-plugin ])
    aws-iam-authenticator
    awscli2-bin
    azure-cli
    bash
    bat
    bat-extras.batman
    bear
    bind
    boost
    btop
    cairo
    ccls
    certigo
    curlFull
    delve
    devenv
    docker
    docker-compose
    dos2unix
    # envsubst # conflicts with gettext
    etcd
    eternal-terminal
    file
    flamegraph
    gcc
    gdb
    gdbm
    ghostscript
    glib
    gmp6
    gnumake
    gnupg
    golangci-lint
    grpcurl
    helix
    htop
    jansson
    just
    k3s
    kluctl
    libsndfile
    mosh
    msgpack
    ncurses
    netperf
    nmap
    openfortivpn
    # openssl # conflicts with libressl
    pinentry
    pkg-config
    redis
    scons
    sd
    ssm-session-manager-plugin
    sysbench
    valgrind
    wezterm
    wireshark
    xsel
    yaml-language-server
  ]
  # npm packages setup via node2nix
  ++ (with npmPackages; [ bash-language-server prettier ]);

  envVars = {
    BAT_THEME = "1337";
    COLORTERM = "truecolor";
    # DOCKER_HOST = "unix:///run/user/1001/podman/podman.sock";
    EDITOR = "hx";
    GO111MODULE = "on";
    GOPATH = (homeDirectory + "/" + goPathSuffix);
    LANG = "en_US.UTF-8";
    LANGUAGE = "en_US.UTF-8";
    LC_ALL = "en_US.UTF-8";
    LESS = "-F -i -M -R -X --incsearch";
    SAML2AWS_USER_AGENT = "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:82.Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:82.0) Gecko/20100101 Firefox/82.00) Gecko/20100101 Firefox/82.0";
    # not working (causes the terminal to freeze):
    # SKIM_TMUX_OPTS = "--color=current_bg:24 --height=40%";
    VISUAL = "hx";
    KUBECONFIG = "/etc/rancher/k3s/k3s.yaml";
  };

  extraPaths = [
    (homeDirectory + "/" + localBinPath)
    (homeDirectory + "/.cargo/bin")
    (homeDirectory + "/" + goPathSuffix + "/bin")
  ];

in
{
  imports = [
    ./common-packages
    (import ./common-shell {
      inherit pkgs goPathSuffix homeDirectory;
    })
    (import ./tmux {
      inherit pkgs lib;
      optionOverrides = [
        {
          name = "window-status-style";
          value = ''
            fg="#888899",bg="#151e24"
          '';
          flags = [ "global" ];
        }
        {
          name = "window-status-last-style";
          value = ''
            fg="#888899",bg="#151e24"
          '';
          flags = [ "global" ];
        }
        {
          name = "window-status-current-style";
          value = ''
            fg="#ccccdd",bg="#4f4f58"
          '';
          flags = [ "global" ];
        }
        {
          name = "status-left";
          value = ''
            "#[bg=#439fad]#[fg=#151e24]#{?client_prefix,#[bg=green],} #S "
          '';
          flags = [ "global" ];
        }
        {
          name = "status-right";
          value = ''
            '#[bg=#202017]#[fg=#585865] %H:%M%Z #(TZ=UTC date +"(%%H:%%MUTC)") '
          '';
          flags = [ "global" ];
        }
        {
          name = "update-environment";
          value = ''
            "SSH_TTY"
          '';
          flags = [ "global" "append" ];
        }
      ];
    })
    (import ./doom {
      inherit pkgs username envVars localBinPath;
      doomDir = doomDirectory;
      emacsPackage = pkgs.emacs-nox;
      launchDaemon = true;
    })
    (import ./git {
      inherit config pkgs lib;
      defaultEmail = "m.sawyer@f5.com";
      defaultUser = "Matt Sawyer";
    })
    ./helix
  ];
  home = {
    inherit homeDirectory;
    inherit username;
    activation = {
      # terminal = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      #   echo 'setting up terminfo for xterm-24bit...'
      #   tic -x -o ~/.terminfo "$HOME/.config/terminfo-24bit.src"
      # '';
      # doom = doomConfig.activation;
      # file."doom.d" = doomConfig.userConfigDir;
    };
    packages = homePackages;
    stateVersion = "22.11";
    # append these extra dirs to the nix-generated path
    sessionPath = extraPaths;
    # make packages available to file.onChange and activation scripts
    extraActivationPath = homePackages;
    sessionVariables = envVars;
    # for git, $EDITOR/$VISUAL can't be set to reference a shell function, so deploy the script as follows
#     file."em.zsh" = {
#       executable = true;
#       source = ./scripts/em.zsh;
#       target = homeDirectory + "/" + localBinPath + "/em";
#     };
#    file."terminfo-24bit.src" = {
#      executable = false;
#      source = ./terminal/terminfo-24bit.src;
#      target = homeDirectory + "/.config/terminfo-24bit.src";
#    };
    file."registries.config" = {
      target = homeDirectory + "/.config/containers/registries.config";
      text = ''
        unqualified-search-registries = ["docker.io", "quay.io","voltera.azurecr.io"]
      '';
    };
    file."policy.json" = {
      target = homeDirectory + "/.config/containers/policy.json";
      text = ''
        {
          "default": [
            {
              "type": "reject"
            }
          ],
          "transports": {
            "dir": {
              "": [
                {
                  "type": "insecureAcceptAnything"
                }
              ]
            },
            "docker": {
              "docker.io": [
                {
                  "type": "insecureAcceptAnything"
                }
              ],
              "volterra.azurecr.io": [
                {
                  "type": "insecureAcceptAnything"
                }
              ]
            }
          }
        }
      '';
    };
  };
  programs.home-manager.enable = true;
  programs.direnv.enable = true;
  programs.skim = {
    enable = true;
    enableZshIntegration = true;
    defaultOptions = [ "--height 40%" ];
  };
  programs.starship = { enable = true; };
  programs.zoxide = { enable = true; };
  programs.zsh = {
    envExtra = builtins.readFile ./.zshenv-haystack;
    initContent = ''
      command -v npm >/dev/null && npm config set prefix ${npmPackagePath} && export PATH=$PATH:$HOME/${npmPackagePath}/bin
      # source <(kubectl completion zsh)
      printf '\e]2;'$(hostname)'\a'
    '';
  };
}
