{ config, pkgs, ... }:

let
  username = "sawyer";
  homeDirectory = "/home/" + username;
  goPathSuffix = "gocode";

in {
  home = {
    inherit homeDirectory;
    inherit username;
    packages = [ ];
    stateVersion = "22.11";
    # append these extra dirs to the nix-generated path
    sessionPath = [
      (homeDirectory + "/.local/bin")
      (homeDirectory + "/.cargo/bin")
      (homeDirectory + "/" + goPathSuffix + "/bin")
    ];
    sessionVariables = {
      AWS_SDK_LOAD_CONFIG = "1";
      GOPATH = (homeDirectory + "/" + goPathSuffix);
      LC_ALL = "en_US.UTF-8";
      LANG = "en_US.UTF-8";
      LANGUAGE = "en_US.UTF-8";
      GO111MODULE = "on";
      BAT_THEME = "1337";
      LESS = "-F -i -M -R -X --incsearch";
      SAML2AWS_USER_AGENT =
        "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:82.Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:82.0) Gecko/20100101 Firefox/82.00) Gecko/20100101 Firefox/82.0";
    };
  };
  programs.home-manager.enable = true;
  programs.direnv.enable = true;
  programs.helix = {
    enable = true;
    settings = {
      theme = "mogster";
      # At most one section each of 'keys.normal', 'keys.insert' and 'keys.select'
      keys.normal = {
        # map vim=like `C` to change from the cursor to the end of the line
        C = [ "collapse_selection" "extend_to_line_end" "change_selection" ];
        # Maps the Control-s to the typable command :w which is an alias for :write (save file)
        "C-s" = ":w";
        # Maps the Control-, to opening of the helix config file
        "C-," = ":open ~/.config/helix/config.toml";
        # Maps the Alt-., to opening of the helix config file
        "A-." = ":open ~/.config/helix/config.toml";
        # Maps the 'w' key move_line_up
        "0" = "goto_line_start";
        # Maps the 'w' key move_line_up
        "$" = "goto_line_end";
        "C-e" = "scroll_down";
        "C-y" = "scroll_up";
        "space" = { ":" = "command_palette"; };
        "#" = "toggle_comments";
        "D" = "kill_to_line_end";
        # # Maps `ga` to show possible code actions
        # g = { a = "code_action"; };
        # # Maps the enter key to open_below then re-enter normal mode
        # "ret" = ["open_below" "normal_mode"];
      };
      # keys.insert = {
      #   # Maps Alt-X to enter normal mode
      #   "A-x" = "normal_mode";
      #   # Maps `jk` to exit insert mode
      #   j = { k = "normal_mode"; };
      # };
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
      mogster = {
        "attribute" = {
          fg = "#dc7759";
          modifiers = [ "bold" ];
        };
        "keyword" = {
          fg = "#dcb659";
          modifiers = [ "bold" ];
        };
        "keyword.directive" = {
          fg = "#dcb659";
          modifiers = [ "bold" ];
        };
        "namespace" = {
          fg = "#d32c5d";
          modifiers = [ "bold" ];
        };
        "punctuation" = "#dc7759";
        "punctuation.delimiter" = "#dc7759";
        "operator" = {
          fg = "#dc7759";
          modifiers = [ "bold" ];
        };
        "special" = "#7fdc59";
        "variable.other.member" = "#c6b8ad";
        "variable" = "#c6b8ad";
        "variable.parameter" = "#c6b8ad";
        "type" = {
          fg = "#dc597f";
          modifiers = [ "bold" ];
        };
        "type.builtin" = {
          fg = "#d32c5d";
          modifiers = [ "bold" ];
        };
        "constructor" = {
          fg = "#dc597f";
          modifiers = [ "bold" ];
        };
        "function" = {
          fg = "#59dcd8";
          modifiers = [ "bold]" ];
        };
        "function.macro" = {
          fg = "#dc7759";
          modifiers = [ "bold" ];
        };
        "function.builtin" = {
          fg = "#59dcd8";
          modifiers = [ "bold" ];
        };
        "comment" = "#627d9d";
        "variable.builtin" = "#c6b8ad";
        "constant" = "#59dcb7";
        "constant.builtin" = "#59dcb7";
        "string" = "#59dcb7";
        "constant.numeric" = "#59c0dc";
        "constant.character.escape" = {
          fg = "#7fdc59";
          modifiers = [ "bold" ];
        };
        "label" = "#59c0dc";
        "module" = "#d32c5d";
        "markup.heading" = "blue";
        "markup.list" = "red";
        "markup.bold" = {
          fg = "yellow";
          modifiers = [ "bold" ];
        };
        "markup.italic" = {
          fg = "magenta";
          modifiers = [ "italic" ];
        };
        "markup.link.url" = {
          fg = "yellow";
          modifiers = [ "underlined" ];
        };
        "markup.link.text" = "red";
        "markup.quote" = "cyan";
        "markup.raw" = "green";
        "diff.plus" = "#59dcb7";
        "diff.delta" = "#dc7759";
        "diff.minus" = "#dc597f";
        "ui.background" = { bg = "#161c23"; };
        "ui.linenr" = { fg = "#415367"; };
        "ui.linenr.selected" = { fg = "#e5ded6"; };
        "ui.statusline" = {
          fg = "#e5ded6";
          bg = "#232d38";
        };
        "ui.statusline.inactive" = {
          fg = "#c6b8ad";
          bg = "#232d38";
        };
        "ui.popup" = { bg = "#232d38"; };
        "ui.window" = { bg = "#232d38"; };
        "ui.help" = {
          bg = "#232d38";
          fg = "#e5ded6";
        };
        "ui.text" = { fg = "#e5ded6"; };
        "ui.text.focus" = {
          fg = "#e5ded6";
          modifiers = [ "bold" ];
        };
        "ui.virtual" = "#627d9d";
        "ui.selection" = { bg = "#313f4e"; };
        "ui.cursor.match" = {
          fg = "#313f4e";
          bg = "#dc7759";
        };
        "ui.cursor" = {
          fg = "#ABB2BF";
          modifiers = [ "reversed" ];
        };
        "ui.menu" = {
          fg = "#e5ded6bg";
          bg = "#232d38";
        };
        "ui.menu.selected" = { bg = "#313f4e"; };
        "warning" = "#dc7759";
        "error" = "#dc597f";
        "info" = "#59dcb7";
        "hint" = "#59c0dc";
        "diagnostic" = {
          fg = "#fbfbfb";
          bg = "#531526";
        };
      }; # mogster
    }; # themes
  }; # helix
  programs.skim = {
    enable = true;
    enableZshIntegration = true;
  };
  programs.starship = { enable = true; };
  programs.tmux = {
    enable = true;
    extraConfig = ''
      # remap prefix to Control + space
      unbind C-b
      set -g mode-keys vi
      bind-key -T copy-mode-vi 'v' send -X begin-selection
      bind-key -T copy-mode-vi 'y' send -X copy-selection-and-cancel
      set -g prefix C-Space
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
      bind-key -n f3 next-layout
      bind-key -n M-J resize-pane -D 5
      bind-key -n M-K resize-pane -U 5
      bind-key -n M-H resize-pane -L 5
      bind-key -n M-L resize-pane -R 5
      bind -n M-Left select-pane -L
      bind -n M-Right select-pane -R
      bind -n M-Up select-pane -U
      bind -n M-Down select-pane -D
      # set -g default-terminal "xterm-24bit"
      # set -ga terminal-overrides ",xterm-24bit:Tc"
      # set -g default-terminal "xterm-256color"
      # set -ga terminal-overrides ",alacritty:Tc"
      set -g automatic-rename off
      set -g focus-events on
      set -g -q mode-mouse on
      set -g -q mouse-resize-pane on
      set -g -q mouse on
      bind -n WheelUpPane if-shell -F -t = "#{mouse_any_flag}" "send-keys -M" "if -Ft= '#{pane_in_mode}' 'send-keys -M' 'copy-mode -e'"
      set -g history-limit 50000
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
      set -g status-right-length 60
      # set -g status-right "#[bg=#444455]#[fg=#bbbbcc] %H:%M "
      set -g status-right ""
      set -g default-command "reattach-to-user-namespace -l zsh"
      set -g status-left "#[bg=#e63634]#[fg=brightwhite]#{?client_prefix,#[bg=green],} #S "
      set -g status-right "#[bg=#444444]#[fg=#888888] #(rainbarf --width 20 --rgb --no-battery --order fciaws)"
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
      ssh = "TERM=xterm-256color ssh";
      socks4proxy = "ssh -D 8888 -f -C -q -N";
      randomizeMacAddress =
        "openssl rand -hex 6 | sed 's/(..)/1:/g; s/.$//' | xargs sudo ifconfig $(route -n get default | grep interface: | cut -d':' -f2 | awk '{print $1}') ether";
      k = "kubectl";
      l = "exa -alF";
      ts = "tmux new-session -n main -s";
      ta = "tmux attach -t";
      tl = "tmux list-sessions";
      em = "em.zsh";
      doom = "~/.emacs.d/bin/doom";
    };
    envExtra = builtins.readFile ./.zshenv-sawyer-dev;
    initExtra = ''
      source <(kubectl completion zsh)
      printf '\e]2;'$(hostname)'\a'
    '';
  };
}
