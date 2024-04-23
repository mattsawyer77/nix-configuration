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
    automake
    aws-iam-authenticator
    awscli2
    azure-cli
    bash
    bat
    bat-extras.batman
    bind
    boost
    cachix
    cairo
    ccls
    certigo
    cmake
    coreutils
    curlFull
    delta
    delve
    diff-so-fancy
    direnv
    docker
    docker-compose
    dos2unix
    envsubst
    etcd
    eternal-terminal
    eza
    fd
    file
    flamegraph
    gdb
    gdbm
    ghostscript
    git
    glib
    gmp6
    gnumake
    gnupg
    go
    golangci-lint
    gopls
    graphviz
    grpcurl
    harfbuzzFull
    helix
    htop
    httrack
    jansson
    jq
    kubectl
    less
    libgccjit
    libiconv
    libsndfile
    libssh2
    libtool
    libvterm-neovim
    libxml2
    llvm
    llvmPackages_12.lldb
    llvm_12
    luajit
    msgpack
    ncurses
    neovim
    netcat
    netperf
    nix-direnv
    nix-prefetch
    nix-prefetch-git
    nix-zsh-completions
    nixfmt
    nmap
    nodejs
    oniguruma
    openapi-generator-cli
    openfortivpn
    openldap
    openssl
    openssl_1_1
    pandoc
    pcre
    pcre2
    pinentry
    pkg-config
    prototool
    python3
    readline
    redis
    ripgrep
    rust-analyzer
    rustup
    scons
    sd
    shared-mime-info
    shellcheck
    skopeo
    sqlite
    ssm-session-manager-plugin
    starship
    taglib
    taplo
    terraform
    terraform-ls
    tflint
    tmux
    tokei
    tree
    unixtools.watch
    upx
    valgrind
    wget
    wireshark
    xsel
    xsv
    yaml-language-server
    youtube-dl
    yq-go
    zellij
    zenith
    zlib
    zoxide
    zsh
    zsh-autosuggestions
    zsh-syntax-highlighting
    zsh-z
    zstd
  ]
  # npm packages setup via node2nix
  ++ (with npmPackages; [ bash-language-server prettier]);

  envVars = {
    BAT_THEME = "1337";
    COLORTERM = "truecolor";
    EDITOR = "em";
    GO111MODULE = "on";
    GOPATH = (homeDirectory + "/" + goPathSuffix);
    LANG = "en_US.UTF-8";
    LANGUAGE = "en_US.UTF-8";
    LC_ALL = "en_US.UTF-8";
    LESS = "-F -i -M -R -X --incsearch";
    SAML2AWS_USER_AGENT = "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:82.Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:82.0) Gecko/20100101 Firefox/82.00) Gecko/20100101 Firefox/82.0";
    # not working (causes the terminal to freeze):
    # SKIM_TMUX_OPTS = "--color=current_bg:24 --height=40%";
    VISUAL = "em"; # see script file below and in scripts/em.zsh
  };

  extraPaths = [
    (homeDirectory + "/" + localBinPath)
    (homeDirectory + "/.cargo/bin")
    (homeDirectory + "/" + goPathSuffix + "/bin")
  ];

  doomConfig = import ./doom/doom.nix { inherit config pkgs lib homeDirectory username envVars; };
in
{
  imports = [
    ./common-packages
    ./common-shell
    ./tmux
    (import ./doom {
      inherit pkgs username envVars;
      doomDir = doomDirectory;
      emacsPackage = pkgs.emacs-nox;
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
      doom = doomConfig.activation;
      file."doom.d" = doomConfig.userConfigDir;
    };
    packages = homePackages;
    stateVersion = "22.11";
    # append these extra dirs to the nix-generated path
    sessionPath = extraPaths;
    # make packages available to file.onChange and activation scripts
    extraActivationPath = homePackages;
    sessionVariables = envVars;
    # install doom config into ~/.doom.d
    # and doom itself into ~/.emacs.d (not a pure install, but this allows us to run doom commands outside nix)
    file."doom.d" = doomConfig.userConfigDir;
    # for git, $EDITOR/$VISUAL can't be set to reference a shell function, so deploy the script as follows
    file."em.zsh" = {
      executable = true;
      source = ./scripts/em.zsh;
      target = homeDirectory + "/" + localBinPath + "/em";
    };
    file."terminfo-24bit.src" = {
      executable = false;
      source = ./terminal/terminfo-24bit.src;
      target = homeDirectory + "/.config/terminfo-24bit.src";
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
  programs.direnv.enable = true;
  programs.skim = {
    enable = true;
    enableZshIntegration = true;
    defaultOptions = [ "--height 40%" ];
  };
  programs.starship = { enable = true; };
  programs.tmux = import ./tmux {
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
  };
  programs.zoxide = { enable = true; };
  programs.zsh = {
    envExtra = builtins.readFile ./.zshenv-sawyer-dev-vio;
    initExtra = ''
      command -v npm >/dev/null && npm config set prefix ${npmPackagePath} && export PATH=$PATH:$HOME/${npmPackagePath}/bin
      source <(kubectl completion zsh)
      printf '\e]2;'$(hostname)'\a'
    '';
  };
}
