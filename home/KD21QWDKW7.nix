{ config, lib, pkgs, username, fontConfig, ... }:

let
  homeDirectory = "/Users/" + username;
  goPathSuffix = "gocode";
  localBinPath = ".local/bin";
  # to update/regenerate, run node2nix -i <(echo '["bash-language-server"]') --nodejs-18
  # then copy the resulting files into ./npm-packages
  npmPackages = import ./npm-packages {
    inherit pkgs;
  };
  homePackages = (with pkgs; [
    alacritty
    automake
    aws-iam-authenticator
    awscli
    azure-cli
    bash
    bat
    bat-extras.batman
    bottom # binary is `btm`
    cachix
    cask
    ccls
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
    exa
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
    (google-cloud-sdk.withExtraComponents [ google-cloud-sdk.components.gke-gcloud-auth-plugin ])
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
    libressl
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
    nixfmt
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
  ]) ++
  (with npmPackages; [ bash-language-server ]);
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
    LESS = "-F -i -M -R -X --incsearch";
    SAML2AWS_USER_AGENT =
      "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:82.Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:82.0) Gecko/20100101 Firefox/82.00) Gecko/20100101 Firefox/82.0";
  };
  extraPaths = [
    (homeDirectory + "/" + localBinPath)
    (homeDirectory + "/.cargo/bin")
    (homeDirectory + "/" + goPathSuffix + "/bin")
  ];
  doomConfig = import ./doom/doom.nix { inherit config pkgs lib username envVars; };
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
      applications="$HOME/Applications"

      # Needs to be writable by the user so that home-manager can create aliases there
      chown ${username} "$applications"
      chmod u+w "$applications"

      for app in ~/Applications/*.app; do
        $DRY_RUN_CMD rm -f "$app"
      done

      find ~/Applications/Home\ Manager\ Apps/* -maxdepth 0 -mindepth 0 -wholename '*.app' -exec readlink '{}' + |
        while read src; do
          rm -f "$"
          # Spotlight does not recognize symlinks, it will ignore directory we link to the applications folder.
          # It does understand MacOS aliases though, a unique filesystem feature. Sadly they cannot be created
          # from bash (as far as I know), so we use the oh-so-great Apple Script instead.
          /usr/bin/osascript -e "
            set fileToAlias to POSIX file \"$src\"
            set applicationsFolder to POSIX file \"$applications\"
            tell application \"Finder\"
              $DRY_RUN_CMD make alias file to fileToAlias at applicationsFolder
              # This renames the alias; 'mpv.app alias' -> 'mpv.app'
              $DRY_RUN_CMD set name of result to \"$(rev <<< "$src" | cut -d'/' -f1 | rev)\"
            end tell
          "
      done
      for app in ~/Applications/*.app; do
        app_name=$(basename "$app" | sd '\.[^\.]+$' $''')
        $DRY_RUN_CMD dockutil --add "$app" --replacing "$app_name" ~${username}
      done
      set +x
    '';
    activation.doom = doomConfig.activation;
    file."doom.d" = doomConfig.userConfigDir;
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
  };
  # programs.git = {
  #   aliases = {
  #     lpg = "log --oneline --graph --format='%C(yellow)%H %<(15)%C(blue)%ci %<(20,trunc)%C(green)%aN %C(reset)%<(100,trunc)%s'";
  #     lp = "log --oneline --format='%C(yellow)%H %C(blue)%ci %C(green)%an %C(reset)%<(100,trunc)%s'";
  #     lt = "log --tags --simplify-by-decoration --format='%C(green)%H %<(15)%C(yellow)%ci %<(20,trunc)%C(cyan)%aN %C(reset)%<(100,trunc)%d%n   %s'";
  #     st = "status -s";
  #   };
  #   delta = {
  #     enable = true;
  #     options = {
  #       paging = "always";
  #       line-numbers = "true";
  #       navigate = "true";
  #       syntax-theme = "zenburn";
  #       width = "1";
  #       minus-style = "syntax \"#450a15\"";
  #       minus-emph-style = "syntax \"#600818\"";
  #       plus-style = "syntax \"#0b4820\"";
  #       plus-emph-style = "syntax \"#175c2e\"";
  #       hunk-header-style = "syntax bold";
  #       hunk-header-decoration-style = "omit";
  #       file-style = "yellow italic";
  #       file-decoration-style = "yellow ul";
  #       line-numbers-zero-style = "#4b5263";
  #       line-numbers-left-format = "\"{nm:^4} \"";
  #       line-numbers-right-format = "\"{np:^4} \"";
  #     };
  #   };
  # };
  programs.home-manager.enable = true;
  programs.alacritty = import ./alacritty/alacritty.nix { inherit fontConfig; };
  programs.direnv.enable = true;
  programs.helix = import ./helix/helix.nix {};
  programs.skim = {
    enable = true;
    enableZshIntegration = true;
  };
  programs.starship = { enable = true; };
  programs.tmux = {
    enable = true;
    extraConfig = ''
      unbind C-b
      # set -g prefix C-l
      set -g prefix C-space
      # bind l send-prefix
      set -g mode-keys vi
      bind-key -T copy-mode-vi 'v' send -X begin-selection
      bind-key -T copy-mode-vi 'y' send -X copy-selection-and-cancel
      set -g base-index 1
      bind-key j command-prompt -p "join pane from:"  "join-pane -hs '%%'"
      bind-key s choose-tree
      bind-key b break-pane
      bind-key c command-prompt -p "new window name:" "new-window -n '%%'"
      bind-key BSpace send-keys " clear && tmux clear-history" \; send-keys "Enter"
      bind-key -n S-Up set-option -g status on
      bind-key -n S-Down set-option -g status off
      bind-key -n S-Left previous-window
      bind-key -n S-Right next-window
      bind-key -n f3 next-layout
      bind-key -n M-J resize-pane -D 5
      bind-key -n M-K resize-pane -U 5
      bind-key -n M-H resize-pane -L 5
      bind-key -n M-L resize-pane -R 5
      bind -n M-Left select-pane -L
      bind -n M-Right select-pane -R
      bind -n M-Up select-pane -U
      bind -n M-Down select-pane -D
      # set -g default-terminal "xterm-256color"
      set -g default-terminal "alacritty"
      # if 'infocmp -x alacritty > /dev/null 2>&1' 'set -g default-terminal "alacritty"'
      set -ag terminal-overrides ",alacritty:RGB"
      set -g automatic-rename off
      set -g focus-events on
      set -g -q mode-mouse on
      set -g -q mouse-resize-pane on
      set -g -q mouse on
      bind -n WheelUpPane if-shell -F -t = "#{mouse_any_flag}" "send-keys -M" "if -Ft= '#{pane_in_mode}' 'send-keys -M' 'copy-mode -e'"
      set -g history-limit 50000
      set -g escape-time 10
      # set pane colors - hilight the active pane
      set -g pane-border-style fg=colour235,bg=black
      set -g pane-active-border-style fg=colour240,bg=black
      # ----------------------
      # Status Bar
      # -----------------------
      set -g status on                       # turn the status bar on
      set -g status-interval 30              # set update frequencey (default 15 seconds)
      set -g status-justify left             # center window list for clarity
      set -g message-style bg=black,fg=green
      set -g status-style bg="#2c2c34",fg=yellow
      set -g window-status-format "  #{window_index}|#{window_name}  "
      set -g window-status-style fg="#888899",bg="#383845"
      set -g window-status-last-style fg="#888899",bg="#383845"
      set -g window-status-current-format "  #{window_index}|#{window_name}  "
      set -g window-status-current-style fg="#ccccdd",bg="#4f4f58"
      set -g status-left-length 70
      set -g status-left "#[bg=#336688]#[fg=#eeeeee] #h #[bg=#113355]#[fg=brightwhite]#{?client_prefix,#[bg=green],} #S "
      # set -g status-right-length 60
      # set -g status-right "#[bg=#444455]#[fg=#bbbbcc] #[fg="#888899"]#[bg="#383845"] %H:%M "
      set -g status-right '#[bg=#202017]#[fg=#585865] %H:%M%Z #(TZ=UTC date +"(%%H:%%MUTC)") '
      # emoji not working:
      # set -g status-right '#[bg=#202017]#[fg=#585865] �🇸� %H: %�� ��M #(TZ=UTC dat +"(%%H:%%UTC �🇦�🇳 #(TCanada/Easternta date +"%%H:%%MUTC ) )  �🇳�🇧 #(TAsia/CalcuttaTC date +"%%H:%%MUTC ) )
      set -g default-command "reattach-to-user-namespace -l zsh"
      set -g status-left "#[bg=#e63634]#[fg=brightwhite]#{?client_prefix,#[bg=green],} #S "
      # set -g status-right "#[bg=#444444]#[fg=#888888] #(rainbarf --width 20 --rgb --no-battery --order fciaws)"
      bind-key y run "tmux save-buffer - | reattach-to-user-namespace pbcopy"
    '';
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
    enableSyntaxHighlighting = true;
    defaultKeymap = "viins";
    history = {
      size = 100000;
      save = 5000;
      ignoreDups = true;
      ignoreSpace = true;
      expireDuplicatesFirst = true;
      extended = true;
    };
    shellAliases = {
      ssh = "TERM=xterm-256color ssh";
      socks4proxy = "ssh -D 8888 -f -C -q -N";
      randomizeMacAddress =
        "openssl rand -hex 6 | sed 's/(..)/1:/g; s/.$//' | xargs sudo ifconfig $(route -n get default | grep interface: | cut -d':' -f2 | awk '{print $1}') ether";
      k = "kubectl";
      kv = "kubectl -n ves-system";
      l = "exa -alF";
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
