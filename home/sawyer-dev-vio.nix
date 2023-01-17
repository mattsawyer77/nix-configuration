{ config, lib, pkgs, ... }:

let
  username = "sawyer";
  homeDirectory = "/home/" + username;
  goPathSuffix = "gocode";
  localBinPath = ".local/bin";

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
    };
    packages = [ ];
    stateVersion = "22.11";
    # append these extra dirs to the nix-generated path
    sessionPath = [
      (homeDirectory + "/" + localBinPath)
      (homeDirectory + "/.cargo/bin")
      (homeDirectory + "/" + goPathSuffix + "/bin")
    ];
    sessionVariables = {
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
      # not working:
      # SKIM_TMUX_OPTS = "--color=current_bg:24 --height=40%";
      VISUAL = "em"; # see script file below and in scripts/em.zsh
    };
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
  programs.tmux = {
    enable = true;
    clock24 = true;
    escapeTime = 0;
    historyLimit = 50000;
    keyMode = "vi";
    prefix = "C-space";
    sensibleOnTop = false;
    extraConfig = ''
      # unbind C-b
      # set -g mode-keys vi
      bind-key -T copy-mode-vi 'v' send -X begin-selection
      bind-key -T copy-mode-vi 'y' send -X copy-selection-and-cancel
      set -g prefix C-space
      set -g base-index 1
      bind Space send-prefix
      bind-key j command-prompt -p "join pane from:"  "join-pane -hs '%%'"
      bind-key s choose-tree
      bind-key b break-pane
      bind-key c command-prompt -p "new window name:" "new-window -n '%%'"
      bind-key BSpace send-keys " clear && tmux clear-history" \; send-keys "Enter"
      bind-key -n S-Up set-option -g status on
      bind-key -n S-Down set-option -g status off
      bind-key -n S-Left previous-window
      bind-key -n S-Right next-window
      bind-key f3 next-layout
      bind-key -n M-J resize-pane -D 5
      bind-key -n M-K resize-pane -U 5
      bind-key -n M-H resize-pane -L 5
      bind-key -n M-L resize-pane -R 5
      bind -n M-Left select-pane -L
      bind -n M-Right select-pane -R
      bind -n M-Up select-pane -U
      bind -n M-Down select-pane -D
      # set -g default-terminal "xterm-256color"
      set -g default-terminal "xterm-24bit"
      # if 'infocmp -x alacritty > /dev/null 2>&1' 'set -g default-terminal "alacritty"'
      set -ag terminal-overrides ",xterm-24bit:RGB"
      set -g automatic-rename off
      set -g focus-events on
      set -g -q mode-mouse on
      set -g -q mouse-resize-pane on
      set -g -q mouse on
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
      set -g window-status-style fg="#888899",bg="#151e24"
      set -g window-status-last-style fg="#888899",bg="#151e24"
      set -g window-status-current-format "  #{window_index}|#{window_name}  "
      set -g window-status-current-style fg="#ccccdd",bg="#4f4f58"
      set -g status-left-length 70
      set -g status-left "#[bg=#ff7e1f]#[fg=#eeeeee] #h "
      set -g status-right '#[bg=#202017]#[fg=#585865] %H:%M%Z #(TZ=UTC date +"(%%H:%%MUTC)") '
      set -g status-left "#[bg=#439fad]#[fg=#151e24]#{?client_prefix,#[bg=green],} #S "
      bind-key y run "tmux save-buffer - | xclip"
      # ensure SSH_TTY env var gets updated automatically when reattaching to a session
      set -ag update-environment "SSH_TTY"
    '';
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
      k = "kubectl";
      l = "exa -alF";
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
