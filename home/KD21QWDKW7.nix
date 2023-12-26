{ config, lib, pkgs, username, fontConfig, mkalias, ... }:

let
  homeDirectory = "/Users/" + username;
  doomDirectory = ".doom.d";
  goPathSuffix = "gocode";
  localBinPath = ".local/bin";
  mkaliasPackage = mkalias.packages.aarch64-darwin.mkalias;
  # to update/regenerate, run node2nix -i <(echo '["bash-language-server", "prettier", "typescript-formatter"]') --nodejs-18
  # then copy the resulting files into ./npm-packages
  npmPackages = import ./npm-packages { inherit pkgs; };
  homePackages = (with pkgs; [
    alacritty
    automake
    aws-iam-authenticator
    awscli
    azure-cli
    bash
    bat
    bat-extras.batman
    bottom # app command is `btm`
    cachix
    cask
    ccls
    certigo
    cmake
    coreutils
    # curlFull # nixpkgs curl builds with openssl 3 which breaks legacy PKCS12 cert auth
    deadcode
    delta
    delve
    # diff-so-fancy
    direnv
    discord
    dockutil
    dos2unix
    editorconfig-core-c
    emacs-mac
    emacs-vterm
    # envsubst # conflicts with gettext which is required for home-manager
    eternal-terminal
    eza
    fd
    findutils
    fontconfig
    fzf
    # gdb # broken as of 2023-01-20
    gdbm
    gettext
    ghostscript
    glib
    # gmp6
    gnumake
    gnupg
    gnuplot
    gnused
    go
    golangci-lint # customized in golangci-lint.nix overlay since it's broken in nixpkgs right now
    (google-cloud-sdk.withExtraComponents
      [ google-cloud-sdk.components.gke-gcloud-auth-plugin ])
    gocyclo
    golint
    gopls
    graphviz
    grpcurl
    gvproxy
    harfbuzzFull
    helix
    html-tidy
    htop
    httrack
    ijq
    isync
    jansson
    jq
    kubectl
    less
    libcxx
    libgccjit
    libiconv
    # libressl
    libsndfile
    libssh2
    libtool
    libvterm-neovim
    libxml2
    # lima
    # llvm
    # llvmPackages_12.lldb
    # llvm_12
    luajit
    # most
    msgpack
    # mu
    # multitail
    # mutagen # broken as of 2022-05-13
    ncurses
    neovim # customized in ./neovim.nix overlay
    netcat
    netperf
    # nim
    # nimlsp
    nil
    ninja
    nix-direnv
    # nix-linter # broken as of 2023-01-04
    nix-prefetch
    nix-prefetch-git
    nix-zsh-completions
    nixpkgs-fmt
    nmap
    node2nix
    nodejs
    oniguruma
    opam
    openapi-generator-cli
    openfortivpn
    openldap
    # openssl
    pandoc
    pcre
    pcre2
    # pdfminer
    pkg-config
    plantuml
    # podman # broken as of 2022-05-12
    # protobuf
    prototool
    python3
    python310Packages.grip
    pywal
    readline
    reattach-to-user-namespace
    # redis
    ripgrep
    rnix-lsp
    rust-analyzer
    rustup
    # scons
    sd
    shared-mime-info
    shellcheck
    shfmt
    skhd
    skim
    skopeo
    starship
    sqlite
    taglib
    taplo
    terraform
    terraform-ls
    tflint
    tmux
    tmuxPlugins.resurrect
    tokei
    tree
    # trivy # broken as of 2022-05-24
    # ttfautohint
    unixtools.watch
    upx
    # vmtouch
    wget
    xsv
    yabai
    yaml-language-server
    yarn
    yj
    youtube-dl
    yq-go
    zlib
    zoxide
    zsh
    zsh-autosuggestions
    zsh-syntax-highlighting
    zsh-z
    zstd
  ])
  # npm packages setup via node2nix
  ++ (builtins.attrValues npmPackages)
  # flakes outside nixpkgs (that don't have overlays)
  # TODO: how to make this more idiomatic without specifying the system arch
  ++ [ mkaliasPackage ];
  envVars = {
    COLORTERM = "truecolor";
    EDITOR = "em";
    VISUAL = "em";
    GOPATH = (homeDirectory + "/" + goPathSuffix);
    USE_GKE_GCLOUD_AUTH_PLUGIN = "True";
    LC_ALL = "en_US.UTF-8";
    LANG = "en_US.UTF-8";
    LANGUAGE = "en_US.UTF-8";
    GO111MODULE = "on";
    BAT_THEME = "1337";
    LESS = "-F -i -M -R -X --incsearch --mouse --wheel-lines 3";
    SAML2AWS_USER_AGENT =
      "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:82.Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:82.0) Gecko/20100101 Firefox/82.00) Gecko/20100101 Firefox/82.0";
  };
  extraPaths = [
    (homeDirectory + "/" + localBinPath)
    (homeDirectory + "/.cargo/bin")
    (homeDirectory + "/" + goPathSuffix + "/bin")
  ];
  doomConfig = import ./doom/doom.nix {
    inherit config pkgs lib homeDirectory username envVars;
    homeDir = homeDirectory;
    doomDir = doomDirectory;
  };
in
{
  home = {
    homeDirectory = homeDirectory;
    packages = homePackages;
    stateVersion = "22.11";
    # append these extra dirs to the nix-generated path
    sessionPath = extraPaths;
    # make packages available to file.onChange and activation scripts
    extraActivationPath = homePackages;
    sessionVariables = envVars;
    # setup application aliases and add them to the Dock
    activation.setupAliases = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      #/usr/bin/env zsh
      set -xe
      echo "setting up ~/Applications..." >&2

      # Needs to be writable by the user so that home-manager can create aliases there
      $DRY_RUN_CMD chown ${username} ~/Applications
      $DRY_RUN_CMD chmod u+w ~/Applications

      for app in ~/Applications/*.app; do
        $DRY_RUN_CMD rm -f "$app"
      done

      find ~/Applications/Home\ Manager\ Apps/* -maxdepth 0 -mindepth 0 -wholename '*.app' -exec readlink '{}' + |
        while read app; do
          # Spotlight does not recognize symlinks, it will ignore directory we link to the applications folder.
          # It does understand MacOS aliases though, a unique filesystem feature. Sadly they cannot be created
          # from bash (as far as I know), so we use a custom utility called mkalias.
          app_name=$(basename "$app" | sd '\.[^\.]+$' $''')
          $DRY_RUN_CMD ${mkaliasPackage}/bin/mkalias $app ~/Applications/$app_name
          $DRY_RUN_CMD ${pkgs.dockutil}/bin/dockutil --add "$app" --replacing "$app_name" ~${username}
      done
      set +x
    '';
    activation.doom = doomConfig.activation;
    file."${doomDirectory}" = doomConfig.userConfigDir;
    # for git, $EDITOR/$VISUAL can't be set to reference a shell function, so deploy the script as follows
    file."em.zsh" = {
      executable = true;
      source = ./scripts/em.zsh;
      target = localBinPath + "/em";
    };
    # install karabiner config (note: this may make the Karabiner Elements app unable to make config changes)
    file."karabiner.json" = {
      text = builtins.toJSON (import ./karabiner.nix);
      target = ".config/karabiner/karabiner.json";
    };
    file.direnv_cache = {
      target = ".config/direnv/direnvrc";
      text = ''
        # Two things to know:
        # * `direnv_layout_dir` is called once for every {.direnvrc,.envrc} sourced
        # * The indicator for a different direnv file being sourced is a different $PWD value
        # This means we can hash $PWD to get a fully unique cache path for any given environment

        : ''${XDG_CACHE_HOME:=$HOME/.cache}
        declare -A direnv_layout_dirs
        direnv_layout_dir() {
          echo "''${direnv_layout_dirs[$PWD]:=$(
            echo -n "$XDG_CACHE_HOME"/direnv/layouts/
            echo -n "$PWD" | sha1sum | cut -d ' ' -f 1
          )}"
        }
      '';
    };
  };
  programs.git = {
    aliases = {
      lpg =
        "log --oneline --graph --format='%C(yellow)%H %<(15)%C(blue)%ci %<(20,trunc)%C(green)%aN %C(reset)%<(100,trunc)%s'";
      lp =
        "log --oneline --format='%C(yellow)%H %C(blue)%ci %C(green)%an %C(reset)%<(100,trunc)%s'";
      lt =
        "log --tags --simplify-by-decoration --format='%C(green)%H %<(15)%C(yellow)%ci %<(20,trunc)%C(cyan)%aN %C(reset)%<(100,trunc)%d%n   %s'";
      st = "status -s";
    };
    delta = {
      enable = true;
      options = {
        paging = "always";
        line-numbers = "true";
        navigate = "true";
        syntax-theme = "zenburn";
        width = "1";
        minus-style = ''syntax "#450a15"'';
        minus-emph-style = ''syntax "#600818"'';
        plus-style = ''syntax "#0b4820"'';
        plus-emph-style = ''syntax "#175c2e"'';
        hunk-header-style = "syntax bold";
        hunk-header-decoration-style = "omit";
        file-style = "yellow italic";
        file-decoration-style = "yellow ul";
        line-numbers-zero-style = "#4b5263";
        line-numbers-left-format = ''"{nm:^4} "'';
        line-numbers-right-format = ''"{np:^4} "'';
      };
    };
  };
  programs.home-manager.enable = true;
  programs.alacritty = import ./alacritty/alacritty.nix { inherit fontConfig; };
  programs.direnv.enable = true;
  programs.helix = import ./helix/helix.nix { };
  programs.skim = {
    enable = true;
    enableZshIntegration = true;
  };
  programs.starship = { enable = true; };
  programs.tmux = import ./tmux {
    inherit pkgs lib;
    optionOverrides = [ ];
  };
  programs.zellij = {
    enable = false;
    settings = {
      default_mode = "locked";
      pane_frames = false;
      scroll_buffer_size = 50000;
      keybinds =
        let
          ctrlQToLocked = {
            key = [{ Ctrl = "l"; }];
            action = [{ SwitchToMode = "locked"; }];
          };
          ctrlQToNormal = {
            key = [{ Ctrl = "l"; }];
            action = [{ SwitchToMode = "normal"; }];
          };
        in
        {
          unbind = [{ Ctrl = "g"; }];
          locked = [ ctrlQToNormal ];
          normal = [ ctrlQToLocked ];
          move = [ ctrlQToLocked ];
          resize = [ ctrlQToLocked ];
          pane = [ ctrlQToLocked ];
          scroll = [ ctrlQToLocked ];
          entersearch = [ ctrlQToLocked ];
          search = [ ctrlQToLocked ];
          renametab = [ ctrlQToLocked ];
          renamepane = [ ctrlQToLocked ];
          session = [ ctrlQToLocked ];
          tab = [
            ctrlQToLocked
            {
              key = [{ Char = "n"; }];
              action = [{ NewTab = { }; } { SwitchToMode = "renametab"; }];
            }
          ];
          # tab = [
          #   { unbind = { Char = "n"; }; }
          #   ctrlQToLocked
          #   {
          #     key = [{ Char = "n"; }];
          #     action = [ { NewTab = { }; } { SwitchToMode = "renametab"; } ];
          #   }
          # ];
        };
      theme = "tokyo-night";
      themes.dracula =
        builtins.fromJSON (builtins.readFile ./zellij/themes/dracula.json);
      themes.gruvbox-dark =
        builtins.fromJSON (builtins.readFile ./zellij/themes/gruvbox-dark.json);
      themes.gruvbox-light = builtins.fromJSON
        (builtins.readFile ./zellij/themes/gruvbox-light.json);
      themes.molokai-dark =
        builtins.fromJSON (builtins.readFile ./zellij/themes/molokai-dark.json);
      themes.nord =
        builtins.fromJSON (builtins.readFile ./zellij/themes/nord.json);
      themes.one-half-dark = builtins.fromJSON
        (builtins.readFile ./zellij/themes/one-half-dark.json);
      themes.solarized-dark = builtins.fromJSON
        (builtins.readFile ./zellij/themes/solarized-dark.json);
      themes.solarized-light = builtins.fromJSON
        (builtins.readFile ./zellij/themes/solarized-light.json);
      themes.tokyo-night-light = builtins.fromJSON
        (builtins.readFile ./zellij/themes/tokyo-night-light.json);
      themes.tokyo-night-storm = builtins.fromJSON
        (builtins.readFile ./zellij/themes/tokyo-night-storm.json);
      themes.tokyo-night =
        builtins.fromJSON (builtins.readFile ./zellij/themes/tokyo-night.json);
    };
  };
  programs.zoxide = { enable = true; };
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    syntaxHighlighting.enable = true;
    defaultKeymap = "viins";
    history = {
      expireDuplicatesFirst = true;
      extended = true;
      ignoreDups = true;
      ignoreSpace = true;
      save = 100000;
      size = 20000000;
    };
    shellAliases = {
      ssh = "TERM=xterm-256color ssh";
      socks4proxy = "ssh -D 8888 -f -C -q -N";
      randomizeMacAddress =
        "openssl rand -hex 6 | sed 's/(..)/1:/g; s/.$//' | xargs sudo ifconfig $(route -n get default | grep interface: | cut -d':' -f2 | awk '{print $1}') ether";
      k = "kubectl";
      kv = "kubectl -n ves-system";
      l = "eza -alF";
      ts = "tmux new-session -n main -s";
      ta = "tmux attach -t";
      tl = "tmux list-sessions";
      doom = "~/.emacs.d/bin/doom";
      zs = "zellij --layout compact --session";
      za = "zellij attach";
      zl = "zellij list-sessions";
    };
    envExtra = builtins.readFile ./.zshenv-KD21QWDKW7.nix;
    initExtra = ''
      source <(kubectl completion zsh)
      printf '\e]2;'$(hostname)'\a'
    '';
  };
}
