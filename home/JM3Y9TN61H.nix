{
  config,
  lib,
  pkgs,
  username,
  fontConfig,
  nixpkgs-stable,
  mcpo,
  mcp-server-tree-sitter,
  duckduckgo-mcp-server,
  emacs-vterm-src,
  ...
}:
let
  homeDirectory = "/Users/${username}";
  doomDirectory = ".doom.d";
  homeAppDirectory = "${homeDirectory}/Applications";
  ghosttyAppDirectory = "${homeAppDirectory}/Ghostty.app";
  localBinPath = ".local/bin";
  npmPackagePath = ".config/npm-packages";
  mcp-server-tree-sitter-package = mcp-server-tree-sitter.packages.aarch64-darwin.default;
  mcpo-package = mcpo.packages.aarch64-darwin.default;
  duckduckgo-mcp-server-package = duckduckgo-mcp-server.packages.aarch64-darwin.default;
  emacs-vterm = import ../modules/emacs-vterm { inherit pkgs emacs-vterm-src; };
  emacs-plus = import ../modules/emacs-plus { inherit pkgs emacs-vterm; };
  localScripts =
    with builtins;
    map
      (
        script:
        let
          scriptName = baseNameOf script;
          scriptContent = readFile script;
        in
        pkgs.writeShellScriptBin scriptName scriptContent
      )
      [
        ./modules/scripts/acr-find-commit
        ./modules/scripts/acr-find-digest
        ./modules/scripts/acr-find-tag
        ./modules/scripts/acr-login
        ./modules/scripts/akarctl
        ./modules/scripts/akardnsctl
        ./modules/scripts/argo2mermaid
        ./modules/scripts/aws-login
        ./modules/scripts/az-login
        ./modules/scripts/check-color
        ./modules/scripts/check-required-tools
        ./modules/scripts/disable-docker-write-through
        ./modules/scripts/docker-shell
        ./modules/scripts/doom-sync-files
        ./modules/scripts/enable-docker-write-through
        ./modules/scripts/env-compass-hostname
        ./modules/scripts/env-gc-site
        ./modules/scripts/env-site-fqdn
        ./modules/scripts/etcd-get-raw
        ./modules/scripts/find-image-data
        ./modules/scripts/find-proto-import-path
        ./modules/scripts/gc-crt-login
        ./modules/scripts/gc-login
        ./modules/scripts/gcr-digest
        ./modules/scripts/gcr-info
        ./modules/scripts/generate-lcov
        ./modules/scripts/generate-protoc-import-dir-locals
        ./modules/scripts/generate-tls-cert
        ./modules/scripts/get-latest-ce-version
        ./modules/scripts/get-f5ai-models
        ./modules/scripts/git-cleanup-branches
        ./modules/scripts/highlight
        ./modules/scripts/hydra-emacs-overlay-revision
        ./modules/scripts/introspect
        ./modules/scripts/kcontainers
        ./modules/scripts/klb
        ./modules/scripts/ksvc
        ./modules/scripts/launchctl-restart
        ./modules/scripts/loki
        ./modules/scripts/matrix-renew-cert
        ./modules/scripts/matrix-renew-certs
        ./modules/scripts/update-opencode-agents
        ./modules/scripts/parse-schema-version
        ./modules/scripts/png2icns
        ./modules/scripts/run-docker
        ./modules/scripts/set-image
        ./modules/scripts/set-input-volume-percent
        ./modules/scripts/setup-ce
        ./modules/scripts/show-docker-cache-mode
        ./modules/scripts/sic-multitrace
        ./modules/scripts/site-public-ips
        ./modules/scripts/site-terraform-output
        ./modules/scripts/skopeo-acr-login
        ./modules/scripts/skopeo-inspect
        ./modules/scripts/skopeo-inspect-commit-log
        ./modules/scripts/skopeo-inspect-digest
        ./modules/scripts/skopeo-inspect-labels
        ./modules/scripts/sre-model-find-commit
        ./modules/scripts/sre-model-update-version
        ./modules/scripts/streak-get-status-objects
        ./modules/scripts/toggle-audio-input-mute
        ./modules/scripts/tz
        ./modules/scripts/wezterm-tab-switcher
        ./modules/scripts/wz
        ./modules/scripts/zoom-autofocus
      ];
  shellScriptWrappers = [
    # enable `gsed` alias which calls gnused for compatibility with homebrew
    (pkgs.writeShellScriptBin "gsed" ''exec ${pkgs.gnused}/bin/sed "$@"'')
    # enable `gsort` alias which calls sort for compatibility with homebrew
    (pkgs.writeShellScriptBin "gsort" ''exec ${pkgs.coreutils}/bin/sort "$@"'')
    # enable `glibtool` alias which calls libtool for compatibility with homebrew
    (pkgs.writeShellScriptBin "glibtool" ''exec ${pkgs.libtool}/bin/libtool "$@"'')
    # enable `gxargs` alias which calls xargs for compatibility with homebrew
    (pkgs.writeShellScriptBin "gxargs" ''exec ${pkgs.findutils}/bin/xargs "$@"'')
    # enable `gtar` alias which calls gnu tar for compatibility with homebrew
    (pkgs.writeShellScriptBin "gtar" ''exec ${pkgs.gnutar}/bin/tar "$@"'')
    (pkgs.writeShellScriptBin "ghostty" ''exec ${ghosttyAppDirectory}/Contents/MacOS/ghostty "$@"'')
    (pkgs.writeShellScriptBin "aws" ''exec /usr/local/bin/aws "$@"'') # remove if awscli becomes fast enough
  ]
  ++ localScripts;
  homePackages =
    with pkgs;
    [
      (google-cloud-sdk.withExtraComponents [ google-cloud-sdk.components.gke-gcloud-auth-plugin ])
      # aws-iam-authenticator
      # awscli2 # too slow, installing from AWS directly for now
      # azure-cli # broken as of 2025-08-29
      bazelisk
      buf
      cachix
      # ccls
      certigo
      cmake
      coreutils
      delve
      devenv
      duf
      dust
      gcov2lcov
      git
      github-mcp-server
      # glab
      gnused
      gnutar
      go
      gocyclo
      golangci-lint
      golint
      # grafana
      # jsonnet
      # jsonnet-language-server
      just
      k9s
      kluctl
      kubecolor
      kubectl
      # llama-cpp
      libiconv
      darwin.libresolv
      # lua
      # lua-language-server
      # mcp-nixos # seems to be broken as of 2025-11-18
      # mockgen
      ncurses
      nix-tree
      # ollama # seems to be broken as of 2025-11-18
      opencode
      # openldap
      pcre
      pkg-config
      # postgresql
      # prometheus
      # prometheus-nats-exporter
      # protols # seems not to work with emacs as of 2025-08-06
      python312
      # python312Packages.chromadb
      # repomix # too old at 1.3.0, install via npm
      ripgrep
      # terraform
      vendir
      watch
      xan
      xq-xml
      yamllint
      # azure-cli # broken on unstable, so using nixpkgs stable
      # colima
      # curlFull # nixpkgs curl builds with openssl 3 which breaks legacy PKCS12 cert auth
      # docker-client
      # etcd # broken as of 2025-03-16?
      # libgccjit
      # open-webui # broken due to python3.12-colbert-ai-0.2.21 being broken on macOS
      # openfortivpn # maybe broken as of 2024-10-31
      # qemu
      # sshfs
      # wireshark # broken as of 2025-10-31
      # xorg.xauth
      # xorg.xhost
      # xquartz # maybe broken as of 2025-11-18
    ]
    # wrappers for homebrew compatibility, etc.
    ++ shellScriptWrappers
    # flakes outside nixpkgs (that don't have overlays)
    # TODO: how to make this more idiomatic without specifying the system arch
    ++ (with nixpkgs-stable.outputs.legacyPackages.aarch64-darwin; [
      azure-cli
      mcpo-package
      mcp-server-tree-sitter-package
      duckduckgo-mcp-server-package
      # wireshark
    ]);
  # ++ (builtins.filter lib.attrsets.isDerivation (builtins.attrValues pkgs.nerd-fonts));
  envVars = {
    EDITOR = "hx";
    VISUAL = "hx";
    USE_GKE_GCLOUD_AUTH_PLUGIN = "True";
    SAML2AWS_USER_AGENT = "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:82.Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:82.0) Gecko/20100101 Firefox/82.00) Gecko/20100101 Firefox/82.0";
    LSP_USE_PLISTS = "true";
    # rancher
    DOCKER_HOST = "unix://$HOME/.rd/docker.sock";
  };
  extraPaths = [
    (homeDirectory + "/" + localBinPath)
    (homeDirectory + "/.cargo/bin")
    (homeDirectory + "/gocode/bin")
    (homeDirectory + "/" + npmPackagePath + "/bin")
    # rancher desktop
    (homeDirectory + "/" + ".rd/bin")
  ];
in
{
  imports = [
    ./modules/common-packages
    ./modules/common-shell
    ./modules/alacritty
    ./modules/ghostty
    ./modules/codex
    ./modules/tmux
    ./modules/doom
    # ./modules/git
    ./modules/helix
    ./modules/powerlevel10k
    ./modules/nats
    ./modules/hammerspoon
    ./modules/opencode
  ];
  custom.alacritty = {
    theme = "kanagawa_wave";
    fontFamily = fontConfig.monospaceFamily;
  };
  custom.ghostty = {
    configDir = "${homeDirectory}/Library/Application Support/com.mitchellh.ghostty";
  };
  custom.tmux = {
    # TODO: is this clobbering tmux config?
    optionOverrides = [
      {
        name = "default-command";
        value = ''"reattach-to-user-namespace -l zsh"'';
        flags = [ "global" ];
      }
    ];
  };
  custom.doom = {
    inherit envVars;
    doomDir = doomDirectory;
    # we'll run doom commands manually
    runDoomCommands = false;
    emacsPackage = emacs-plus;
    installEmacs = true;
    inherit fontConfig;
  };
  # custom.git = {
  #   defaultEmail = "m.sawyer@f5.com";
  #   defaultUser = "Matt Sawyer";
  # };
  custom.opencode = {
    settings = {
      "$schema" = "https://opencode.ai/config.json";
      # plugin = [ "@tarquinen/opencode-dcp@latest" ];
      provider = {
        f5ai-anthropic = {
          models = {
            claude-opus-4-6 = {
              limit = {
                context = 1000000;
                output = 128000;
              };
              modalities = {
                input = [
                  "text"
                  "image"
                ];
                output = [ "text" ];
              };
              name = "F5AI: Claude Opus 4.6";
              options = {
                thinking = {
                  type = "adaptive";
                };
              };
              reasoning = true;
              variants = {
                off = {
                  thinking = {
                    type = "disabled";
                  };
                };
              };
            };
            claude-sonnet-4-6 = {
              limit = {
                context = 1000000;
                output = 128000;
              };
              modalities = {
                input = [
                  "text"
                  "image"
                ];
                output = [ "text" ];
              };
              name = "F5AI: Claude Sonnet 4.6";
              options = {
                thinking = {
                  type = "adaptive";
                };
              };
              reasoning = true;
              variants = {
                off = {
                  thinking = {
                    type = "disabled";
                  };
                };
              };
            }; # sonnet 4.6
            claude-haiku-4-5 = {
              limit = {
                context = 200000;
                output = 64000;
              };
              modalities = {
                input = [
                  "text"
                  "image"
                ];
                output = [ "text" ];
              };
              name = "F5AI: Claude Haiku 4.5";
            }; # haiku 4.5
          }; # anthropic models
          name = "F5AI (Anthropic)";
          npm = "@ai-sdk/anthropic";
          options = {
            baseURL = "https://f5ai.pd.f5net.com/anthropic/v1";
            headers = {
              anthropic-beta = "interleaved-thinking-2025-05-14";
            };
          };
        }; # anthropic provider
        f5ai-openai = {
          models = {
            "gpt-5.3-codex" = {
              limit = {
                context = 272000;
                output = 128000;
              };
              modalities = {
                input = [
                  "text"
                  "image"
                ];
                output = [ "text" ];
              };
              name = "F5AI: GPT 5.3 Codex";
              reasoning = true;
              release_date = "2026-02-01";
            }; # codex 5.3
            "gpt-5.4" = {
              limit = {
                context = 200000;
                output = 128000;
              };
              modalities = {
                input = [
                  "text"
                  "image"
                ];
                output = [ "text" ];
              };
              name = "F5AI: GPT 5.4";
              reasoning = true;
              release_date = "2026-03-01";
            }; # gpt 5.4
          }; # openai models
          name = "F5AI (OpenAI)";
          npm = "@ai-sdk/openai";
          options = {
            baseURL = "https://f5ai.pd.f5net.com/openai";
          };
        }; # openai provider
        # f5ai = {
        #   name = "f5ai";
        #   options = {
        #     baseURL = "https://f5ai.pd.f5net.com/openai";
        #   };
        #   models = {
        #     "claude-opus-4-6" = {
        #       name = "F5AI: Claude Opus 4.6";
        #     };
        #     "claude-sonnet-4-6" = {
        #       name = "F5AI: Claude Sonnet 4.6";
        #     };
        #     "gpt-5.4" = {
        #       name = "F5AI: GPT 5.4";
        #     };
        #   };
        #   npm = "@ai-sdk/openai-compatible";
        # };
      };
      instructions = [
        "*/AGENTS.md"
        "AGENTS.md"
        "README.md"
        "*/README.md"
        "pbdoc/docs.md"
      ];
      compaction = {
        auto = true;
        prune = true;
      };
      # mcp = {
      # # TODO: figure out how to set these securely
      #   atlassian = {
      #     command = [
      #       "uvx"
      #       "mcp-atlassian"
      #     ];
      #     environment = {
      #       CONFLUENCE_PERSONAL_TOKEN = "{env:F5_CONFLUENCE_PAT}";
      #       CONFLUENCE_SSL_VERIFY = "false";
      #       CONFLUENCE_URL = "{env:F5_CONFLUENCE_URL}";
      #       JIRA_PERSONAL_TOKEN = "{env:F5_JIRA_PAT}";
      #       JIRA_SSL_VERIFY = "false";
      #       JIRA_URL = "{env:F5_JIRA_URL}";
      #     };
      #     type = "local";
      #   };
      #   git = {
      #     command = [
      #       "uvx"
      #       "mcp-server-git"
      #       "--repository"
      #       "\${PWD}"
      #     ];
      #     type = "local";
      #   };
      # };
    };

    # ── SANDBOX CONFIGURATION ──────────────────────────────────────────
    # OS-level sandboxing via Anthropic's srt (sandbox-runtime)
    # Uses macOS Seatbelt for filesystem isolation + proxy for network filtering
    sandbox = {
      enable = true;
      filesystem = {
        # Sensitive directories to block reads from
        denyRead = [
          "~/.ssh"
          "~/.gnupg"
          "~/.aws"
          "~/.azure"
          "~/.kube"
          "~/.docker"
          "~/.netrc"
          "~/.git-credentials"
          "~/.config/gh"
          "~/.config/glab"
        ];

        # Paths where writes are allowed (default: cwd + /tmp)
        # opencode needs write access to its data dir for logs, db, snapshots
        allowWrite = [
          "."
          "/tmp"
          "~/.local/share/opencode"
          # "~/.config/opencode"
        ];

        # Files to block writes to even within allowed paths
        denyWrite = [
          ".env"
          ".env.*"
          "*.pem"
          "*.key"
          "*.p12"
          "*.pfx"
          "credentials.json"
          "secrets.yaml"
          "secrets.yml"
        ];
      };

      network = {
        # Only these domains can be accessed from within the sandbox
        allowedDomains = [
          # LLM API endpoint
          "f5ai.pd.f5net.com"
          # opencode updates/schema
          "opencode.ai"
          "*.opencode.ai"
          # Git operations
          "github.com"
          "*.github.com"
          "gitlab.com"
          "*.gitlab.com"
          # npm for srt itself
          "registry.npmjs.org"
          "*.npmjs.org"
          "docs.f5net.com"
          "duckduckgo.com"
        ];
      };
    };
  };

  targets.darwin = {
    linkApps.enable = false;
    copyApps.enable = true;
  };
  home = {
    homeDirectory = homeDirectory;
    packages = homePackages;
    stateVersion = "22.11";
    # append these extra dirs to the nix-generated path
    sessionPath = extraPaths;
    sessionVariables = envVars;
    file.".gitconfig" = {
      source = ./modules/git/config;
      target = homeDirectory + "/.config/git/config";
      force = true;
    };
    file.".gitignore" = {
      source = ./modules/git/ignore;
      target = homeDirectory + "/.config/git/ignore";
      force = true;
    };
  };
  programs.home-manager.enable = true;
  programs.zsh = {
    shellAliases = {
      sia = "nohup ~/Applications/sia.app/Contents/MacOS/sia >/dev/null 2>&1 &";
    };
    envExtra = builtins.readFile ./.zshenv-JM3Y9TN61H;
    initContent = ''
      # hack to fix emacs/eat
      if [ -n "$INSIDE_EMACS" ]; then
        if [[ $INSIDE_EMACS =~ "eat" ]]; then
          export TERM=eat-color
        elif [[ $INSIDE_EMACS =~ "vterm" ]]; then
          export TERM=xterm-256color
        else
          export TERM=xterm
        fi
        # disable vi key bindings
        bindkey -e
      fi
      export POWERLEVEL9K_CONFIG_FILE=~/workspaces/nix-configuration/home/powerlevel10k/.p10k.zsh
      # Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
      # Initialization code that may require console input (password prompts, [y/n]
      # confirmations, etc.) must go above this block; everything else may go below.
      if [[ -r "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh" ]]; then
        source "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh"
      fi
      if command -v aws_completer >/dev/null; then
        complete -C 'aws_completer' aws
      fi
      command -v npm >/dev/null && npm config set prefix ${npmPackagePath} && export PATH=$PATH:$HOME/${npmPackagePath}/bin
    '';
  };
}
