{ config, lib, pkgs, ... }:

let
  username = "sawyer";
  homeDirectory = "/home/" + username;
  goPathSuffix = "gocode";
  localBinPath = ".local/bin";
  # to update/regenerate, run node2nix -i <(echo '["bash-language-server"]') --nodejs-18
  # then copy the resulting files into ./npm-packages
  npmPackages = import ./npm-packages { inherit pkgs; };
  homePackages = with pkgs; [
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
    # cask
    ccls
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
    # emacsGit-nox
    emacs-nixos # from overlay below
    emacs-vterm
    envsubst
    etcd
    eternal-terminal
    eza
    fd
    file
    flamegraph
    fzf
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
    (google-cloud-sdk.withExtraComponents
      [ google-cloud-sdk.components.gke-gcloud-auth-plugin ])
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
    # nix-linter # broken as of 2023-01-17
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
    # podman
    # protobuf
    # protobuf3_11
    prototool
    python3
    readline
    redis
    ripgrep
    rnix-lsp
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
  ++ (with npmPackages; [ bash-language-server ]);

  envVars = {
    BAT_THEME = "1337";
    COLORTERM = "truecolor";
    EDITOR = "em";
    GO111MODULE = "on";
    GOPATH = (homeDirectory + "/" + goPathSuffix);
    LANG = "en_US.UTF-8";
    LANGUAGE = "en_US.UTF-8";
    LC_ALL = "en_US.UTF-8";
    LESS = "-F -i -M -R -X --incsearch --mouse --wheel-lines 3";
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
  home = {
    inherit homeDirectory;
    inherit username;
    activation = {
      terminal = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        echo 'setting up terminfo for xterm-24bit...'
        tic -x -o ~/.terminfo "$HOME/.config/terminfo-24bit.src"
      '';
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
  programs.helix = {
    enable = true;
    settings = {
      # theme = "ayu_dark";
      theme = "mogster";
      # theme = "edge";
      # theme = "everforest";
      # theme = "gruvbox";
      # theme = "mogster";
      # theme = "sonokai";
      keys.normal = {
        "#" = "toggle_comments";
        "$" = "goto_line_end";
        "0" = "goto_line_start";
        "{" = [ "goto_prev_paragraph" ];
        "}" = [ "goto_next_paragraph" ];
        b = [ "move_prev_word_start" "collapse_selection" ];
        d = {
          a = [ "select_textobject_around" ];
          d = [ "extend_to_line_bounds" "delete_selection" ];
          i = [ "select_textobject_inner" ];
          s = [ "surround_delete" ];
          t = [ "extend_till_char" ];
        };
        e = [ "move_next_word_end" "collapse_selection" ];
        C = [ "collapse_selection" "extend_to_line_end" "change_selection" ];
        C-e = "scroll_down";
        C-n = "select_next_sibling";
        C-p = "select_prev_sibling";
        C-s = ":w";
        C-y = "scroll_up";
        D = "kill_to_line_end";
        G = "goto_file_end";
        space = { ":" = "command_palette"; };
        tab = "match_brackets";
        V = [ "select_mode" "extend_to_line_bounds" ];
        w = [ "move_next_word_start" "move_char_right" "collapse_selection" ];
        x = "delete_selection";
        y = {
          y = [
            "select_mode"
            "extend_to_line_bounds"
            "yank_main_selection_to_clipboard"
            "normal_mode"
          ];
        };
      };
      keys.select = {
        "0" = "goto_line_start";
        "$" = [ "goto_line_end" ];
        d = [ "yank_main_selection_to_clipboard" "delete_selection" ];
        esc = [ "collapse_selection" "keep_primary_selection" "normal_mode" ];
        j = [ "extend_line_down" "extend_to_line_bounds" ];
        k = [ "extend_line_up" "extend_to_line_bounds" ];
        p = "replace_selections_with_clipboard";
        P = "paste_clipboard_before";
        tab = "match_brackets";
        v = "expand_selection";
        V = "shrink_selection";
        x = [ "yank_main_selection_to_clipboard" "delete_selection" ];
        y = [
          "yank_main_selection_to_clipboard"
          "normal_mode"
          "flip_selections"
          "collapse_selection"
        ];
        Y = [
          "extend_to_line_bounds"
          "yank_main_selection_to_clipboard"
          "goto_line_start"
          "collapse_selection"
          "normal_mode"
        ];
      };
      editor = {
        file-picker = { hidden = false; };
        lsp = { display-messages = true; };
        cursor-shape = {
          insert = "bar";
          normal = "block";
        };
      };
    }; # settings
    languages = [{
      name = "go";
      indent = {
        tab-width = 2;
        unit = "  ";
      };
    }]; # languages
    themes = {
      edge = (builtins.fromJSON (builtins.readFile ./helix/themes/edge.json));
      everforest = (builtins.fromJSON (builtins.readFile ./helix/themes/everforest.json));
      gruvbox = (builtins.fromJSON (builtins.readFile ./helix/themes/gruvbox.json));
      mogster = (builtins.fromJSON (builtins.readFile ./helix/themes/mogster.json));
      sonokai = (builtins.fromJSON (builtins.readFile ./helix/themes/sonokai.json));
    }; # themes
  }; # helix
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
  # programs.tmux = {
  #   enable = true;
  #   clock24 = true;
  #   escapeTime = 0;
  #   historyLimit = 50000;
  #   keyMode = "vi";
  #   prefix = "C-space";
  #   sensibleOnTop = false;
  #   extraConfig = ''
  #     # unbind C-b
  #     # set -g mode-keys vi
  #     bind-key -T copy-mode-vi 'v' send -X begin-selection
  #     bind-key -T copy-mode-vi 'y' send -X copy-selection-and-cancel
  #     set -g prefix C-space
  #     set -g base-index 1
  #     bind Space send-prefix
  #     bind-key j command-prompt -p "join pane from:"  "join-pane -hs '%%'"
  #     bind-key s choose-tree
  #     bind-key b break-pane
  #     bind-key c command-prompt -p "new window name:" "new-window -n '%%'"
  #     bind-key BSpace send-keys " clear && tmux clear-history" \; send-keys "Enter"
  #     bind-key -n S-Up set-option -g status on
  #     bind-key -n S-Down set-option -g status off
  #     bind-key -n S-Left previous-window
  #     bind-key -n S-Right next-window
  #     bind-key f3 next-layout
  #     bind-key -n M-J resize-pane -D 5
  #     bind-key -n M-K resize-pane -U 5
  #     bind-key -n M-H resize-pane -L 5
  #     bind-key -n M-L resize-pane -R 5
  #     bind -n M-Left select-pane -L
  #     bind -n M-Right select-pane -R
  #     bind -n M-Up select-pane -U
  #     bind -n M-Down select-pane -D
  #     # set -g default-terminal "xterm-256color"
  #     set -g default-terminal "xterm-24bit"
  #     if 'infocmp -x alacritty > /dev/null 2>&1' 'set -g default-terminal "alacritty"'
  #     set -ag terminal-overrides ",xterm-24bit:RGB"
  #     set -g automatic-rename off
  #     set -g focus-events on
  #     set -g -q mode-mouse on
  #     set -g -q mouse-resize-pane on
  #     set -g -q mouse on
  #     set -g history-limit 50000
  #     set -g escape-time 10
  #     # set pane colors - hilight the active pane
  #     set -g pane-border-style fg=colour235,bg=black
  #     set -g pane-active-border-style fg=colour240,bg=black
  #     # ----------------------
  #     # Status Bar
  #     # -----------------------
  #     set -g status-left-length 70
  #     set -g window-status-format "  #{window_index}|#{window_name}  "
  #     set -g window-status-current-format "  #{window_index}|#{window_name}  "
  #     set -g status on                       # turn the status bar on
  #     set -g status-interval 30              # set update frequencey (default 15 seconds)
  #     set -g status-justify left             # center window list for clarity
  #     set -ga window-active-style bg="#1b1f24",fg="#afc0d9"
  #     set -ga window-style bg="#1b1f24",fg="#495d7c"
  #     set -g message-style bg=black,fg=green
  #     set -g status-style bg="#2c2c34",fg=yellow
  #     set -g window-status-style fg="#888899",bg="#151e24"
  #     set -g window-status-last-style fg="#888899",bg="#151e24"
  #     set -g window-status-current-style fg="#ccccdd",bg="#4f4f58"
  #     set -g status-left "#[bg=#439fad]#[fg=#151e24]#{?client_prefix,#[bg=green],} #S "
  #     set -g status-right '#[bg=#202017]#[fg=#585865] %H:%M%Z #(TZ=UTC date +"(%%H:%%MUTC)") '
  #     bind-key y run "tmux save-buffer - | xclip"
  #     # ensure SSH_TTY env var gets updated automatically when reattaching to a session
  #     set -ag update-environment "SSH_TTY"
  #   '';
  # };
  programs.zoxide = { enable = true; };
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    syntaxHighlighting.enable = true;
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
      k = "kubectl";
      l = "eza -alF";
      ts = "tmux new-session -n main -s";
      ta = "tmux attach -t";
      tl = "tmux list-sessions";
      doom = "~/.emacs.d/bin/doom";
    };
    envExtra = builtins.readFile ./.zshenv-sawyer-dev-vio;
    initExtra = ''
      source <(kubectl completion zsh)
      printf '\e]2;'$(hostname)'\a'
    '';
  };
}
