{ config, lib, pkgs, nixpkgs-stable, username, ... }:

let
  homeDirectory = "/home/" + username;
  doomDirectory = ".doom.d";
  goPathSuffix = "gocode";
  localBinPath = ".local/bin";
  npmPackagePath = ".config/npm-packages";
  # to update/regenerate, run node2nix -i <(echo '["bash-language-server", "prettier"]') --nodejs-18
  # then copy the resulting files into ./npm-packages
  # npmPackages = import ./npm-packages { inherit pkgs; };
  homePackages = (with pkgs; [
    (google-cloud-sdk.withExtraComponents [ google-cloud-sdk.components.gke-gcloud-auth-plugin ])
    alacritty
    aws-iam-authenticator
    awscli2
    # azure-cli # broken with 2.78.0 ("claims_challenge" error)
    bash
    bat
    bat-extras.batman
    bear
    bind
    boost
    btop
    buildkit
    # cairo
    ccls
    certigo
    delve
    devenv
    dos2unix
    duf
    dust
    emacs-nox
    # envsubst # conflicts with gettext
    firefox
    # flamegraph
    gcc
    # gdb
    gdbm
    ghostscript
    glib
    gmp6
    gnumake
    gnupg
    golangci-lint
    grpcurl
    helix
    # kubernetes-helm
    (wrapHelm kubernetes-helm {
        plugins = with pkgs.kubernetes-helmPlugins; [
          helm-secrets
          helm-diff
          helm-s3
          helm-git
        ];
      })
    # htop
    # jansson
    just
    jq
    kluctl
    # libcgroup
    libsndfile
    llama-cpp
    k9s
    # mcphost
    # mosh
    # msgpack
    natscli
    ncurses
    nerdctl
    netperf
    # openfortivpn
    # openssl # conflicts with libressl
    pinentry-curses
    postgresql
    python3
    # redis
    # repomix
    # scons
    # ssm-session-manager-plugin
    # sysbench
    tailscale
    # valgrind
    # wezterm
    # wireshark
    xsel
    yaml-language-server
  ]) ++ (with nixpkgs-stable.outputs.legacyPackages.x86_64-linux; [
    azure-cli
  ]);
  # npm packages setup via node2nix
  # ++ (with npmPackages; [ bash-language-server prettier ]);

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
    MOZ_ENABLE_WAYLAND = "0";
    GDK_BACKEND = "x11";
    # Optional: avoid GL issues on headless servers
    LIBGL_ALWAYS_SOFTWARE = "1";
    MOZ_WEBRENDER = "0";
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
            "#[bg=#E8993E]#[fg=#151e24]#{?client_prefix,#[bg=green],} #S "
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
      emacsPackage = pkgs.emacs;
      launchDaemon = false;
      runDoomCommands = false;
    })
    (import ./git {
      inherit config pkgs lib;
      defaultEmail = "m.sawyer@f5.com";
      defaultUser = "Matt Sawyer";
    })
    ./helix
    ./ollama
    (import ./opencode {
      settings = {
        "$schema" = "https://opencode.ai/config.json";
        provider = {
          f5ai = {
            name = "f5ai";
            options = {
              baseURL = "https://f5ai.pd.f5net.com/api";
            };
            models = {
              "claude-opus-4-6" = {
                name = "F5AI: Claude Opus 4.6";
              };
              "claude-sonnet-4-6" = {
                name = "F5AI: Claude Sonnet 4.6";
              };
            };
            npm = "@ai-sdk/openai-compatible";
          };
        };
      };
    })
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
      if [ -z "$SSH_TTY" ] && [ -n "$ET_VERSION" ]; then
        export SSH_TTY=$(tty)
      fi
      printf '\e]2;'$(hostname)'\a'
    '';
  };
}
