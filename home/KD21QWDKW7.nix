{ lib
, pkgs
, nixpkgs-stable
# , nixpkgs-emacs
, username
, mcpo
, mcp-server-tree-sitter
, duckduckgo-mcp-server
# , wezterm
# , ghostty
, ...
}:

let
  homeDirectory = "/Users/" + username;
  doomDirectory = ".doom.d";
  homeAppDirectory = "${homeDirectory}/Applications";
  emacsAppDirectory = "${homeAppDirectory}/Emacs.app";
  ghosttyAppDirectory ="${homeAppDirectory}/Ghostty.app";
  goPathSuffix = "gocode";
  localBinPath = ".local/bin";
  npmPackagePath = ".config/npm-packages";
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
    # enable `emacs` alias which calls Emacs
    (pkgs.writeShellScriptBin "emacs" ''exec ${emacsAppDirectory}/Contents/MacOS/Emacs "$@"'')
    (pkgs.writeShellScriptBin "ghostty" ''exec ${ghosttyAppDirectory}/Contents/MacOS/ghostty "$@"'')
    (pkgs.writeShellScriptBin "aws" ''exec /usr/local/bin/aws "$@"'') # remove if awscli becomes fast enough
  ] ++ localScripts;
  homePackages = (with pkgs; [
    (google-cloud-sdk.withExtraComponents [ google-cloud-sdk.components.gke-gcloud-auth-plugin ])
    aws-iam-authenticator
    # awscli2 # too slow, installing from AWS directly for now
    # azure-cli # broken as of 2025-08-29
    bazelisk
    buf
    cachix
    ccls
    certigo
    cmake
    coreutils
    delve
    devenv
    duf
    dust
    git
    github-mcp-server
    glab
    gnused
    gnutar
    go_1_24 # need to test if go 1.25 is causing slowdowns
    gocyclo
    golangci-lint
    golint
    jsonnet
    jsonnet-language-server
    just
    k9s
    kluctl
    kubecolor
    kubectl
    libiconv
    # mcp-nixos # seems to be broken as of 2025-11-18
    mockgen
    ncurses
    nix-tree
    # ollama # seems to be broken as of 2025-11-18
    opencode
    openldap
    pcre
    pkg-config
    python312
    # python312Packages.chromadb
    # repomix # too old at 1.3.0, install via npm
    ripgrep
    terraform
    vendir
    watch
    # wireshark # broken as of 2025-10-31
    xan
    xorg.xauth
    xorg.xhost
    xq-xml
    # xquartz # maybe broken as of 2025-11-18
    yamllint
    # aider-chat
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
  ])
  # wrappers for homebrew compatibility, etc.
  ++ shellScriptWrappers
  # flakes outside nixpkgs (that don't have overlays)
  # TODO: how to make this more idiomatic without specifying the system arch
  ++ (with nixpkgs-stable.outputs.legacyPackages.aarch64-darwin; [
    azure-cli
    mcpo-package
    mcp-server-tree-sitter-package
    duckduckgo-mcp-server-package
    wireshark
  ]);
  # ++ (builtins.filter lib.attrsets.isDerivation (builtins.attrValues pkgs.nerd-fonts));
  envVars = {
    EDITOR = "hx";
    VISUAL = "hx";
    USE_GKE_GCLOUD_AUTH_PLUGIN = "True";
    SAML2AWS_USER_AGENT =
      "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:82.Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:82.0) Gecko/20100101 Firefox/82.00) Gecko/20100101 Firefox/82.0";
    LSP_USE_PLISTS = "true";
    EMACS = "${emacsAppDirectory}/Contents/MacOS/Emacs";
  };
  extraPaths = [
    (homeDirectory + "/" + localBinPath)
    (homeDirectory + "/.cargo/bin")
    # (homeDirectory + "/.docker/bin")
    # "/Applications/Docker.app/Contents/Resources/bin"
    (homeDirectory + "/" + goPathSuffix + "/bin")
    (homeDirectory + "/" + npmPackagePath + "/bin")
    (emacsAppDirectory + "/Content/MacOS")
    # rancher desktop
    (homeDirectory + "/" + ".rd/bin")
  ];
in
{
  imports = [
    ./common-packages
    (import ./common-shell {
      inherit pkgs homeDirectory goPathSuffix;
    })
    # (import ./wezterm {
    #   inherit pkgs;
    #   # weztermPackage = nixpkgs-stable.outputs.legacyPackages.aarch64-darwin.wezterm;
    # })
    (import ./alacritty {
      # seems there are some breaking changes in config, so using stable for now
      # pkgs = nixpkgs-stable.outputs.legacyPackages.aarch64-darwin;
      inherit pkgs;
      theme = "kanagawa_wave";
    })
    (import ./ghostty {
      # ghosttyPackage = ghostty.outputs.packages.aarch64-darwin.ghostty;
      inherit pkgs lib;
      configDir = "${homeDirectory}/Library/Application Support/com.mitchellh.ghostty";
    })
    # while wezterm is having some flakiness
    (import ./tmux {
      inherit pkgs;
      optionOverrides = [
        {
          name = "default-command";
          value = ''"reattach-to-user-namespace -l zsh"'';
          flags = [ "global" ];
        }
      ];
    })
    ./karabiner
    (import ./doom {
      inherit lib pkgs username envVars;
      doomDir = doomDirectory;
      # we'll run doom commands manually
      runDoomCommands = false;
      # emacsPackage = pkgs.emacs29-macport;
      # disabled while trying out jimeh build installed externally
      # emacsPackage = nixpkgs-emacs.outputs.legacyPackages.aarch64-darwin.emacs29-macport;
    })
    # (import ./git {
    #   inherit config pkgs lib;
    #   defaultEmail = "m.sawyer@f5.com";
    #   defaultUser = "Matt Sawyer";
    # })
    ./helix
    # ./broot
    # ./aider
    ./powerlevel10k
    ./nats
  ];
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
      source = ./git/config;
      target = homeDirectory + "/.config/git/config";
      force = true;
    };
    file.".gitignore" = {
      source = ./git/ignore;
      target = homeDirectory + "/.config/git/ignore";
      force = true;
    };
  };
  programs.home-manager.enable = true;
  programs.opencode = {
    enable = true;
    settings = {
      "$schema" = "https://opencode.ai/config.json";
      provider = {
        f5ai = {
          name = "f5ai";
          options = {
            baseURL = "https://f5ai.pd.f5net.com/api";
          };
          models = {
            "gpt-4.1" = {
              name = "F5AI: GPT 4.1";
            };
            "gpt-5" = {
              name = "F5AI: GPT 5";
            };
            "gpt-5-codex" = {
              name = "F5AI: GPT 5 Codex";
            };
          };
          npm = "@ai-sdk/openai-compatible";
        };
      };
    };
  };
  programs.zsh = {
    shellAliases = {
      sia = "nohup ~/Applications/sia.app/Contents/MacOS/sia >/dev/null 2>&1 &";
      tailscale = "/Applications/Tailscale.app/Contents/MacOS/Tailscale";
    };
    envExtra = builtins.readFile ./.zshenv-KD21QWDKW7;
    initContent = ''
    # hack to fix emacs/eat
    if [ -n "$INSIDE_EMACS" ]; then
      if [[ $INSIDE_EMACS =~ "eat" ]]; then
        export TERM=eat-color
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
    autoload -Uz compinit && compinit
    if command -v aws_completer >/dev/null; then
      complete -C 'aws_completer' aws
    fi
    if command -v kubectl >/dev/null; then
      source <(kubectl completion zsh)
      # alias has to be setup after the above source for completion to work
      alias k=kubectl
    fi
    command -v npm >/dev/null && npm config set prefix ${npmPackagePath} && export PATH=$PATH:$HOME/${npmPackagePath}/bin
    '';
  };
}
