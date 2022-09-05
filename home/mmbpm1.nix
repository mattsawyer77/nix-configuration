{ config, pkgs, ... }:

{
  home.homeDirectory = "/Users/matt";
  home.packages = [ ];
  home.stateVersion = "22.11";
  home.username = "matt";
  programs.home-manager.enable = true;
  programs.alacritty = {
    enable = true;
    settings = {
      live_config_reload = true;
      # colors = builtins.fromJSON (builtins.readFile ./alacritty-themes/Afterglow.json);
      # colors = builtins.fromJSON (builtins.readFile ./alacritty-themes/Argonaut.json);
      # colors = builtins.fromJSON (builtins.readFile ./alacritty-themes/Atelierlakeside.dark.json);
      # colors = builtins.fromJSON (builtins.readFile ./alacritty-themes/Ayu-Dark.json);
      # colors = builtins.fromJSON (builtins.readFile ./alacritty-themes/Ayu-Mirage.json);
      # colors = builtins.fromJSON (builtins.readFile ./alacritty-themes/Brewer.dark.json);
      # colors = builtins.fromJSON (builtins.readFile ./alacritty-themes/Eqie6.json);
      # colors = builtins.fromJSON (builtins.readFile ./alacritty-themes/Hybrid.json);
      # colors = builtins.fromJSON (builtins.readFile ./alacritty-themes/Iceberg-Dark.json);
      # colors = builtins.fromJSON (builtins.readFile ./alacritty-themes/Ocean.dark.json);
      # colors = builtins.fromJSON (builtins.readFile ./alacritty-themes/Palenight.json);
      colors = builtins.fromJSON (builtins.readFile ./alacritty-themes/Tokyonight_Night.json);
      # colors = builtins.fromJSON (builtins.readFile ./alacritty-themes/Twilight.dark.json);
      # colors = builtins.fromJSON (builtins.readFile ./alacritty-themes/github_dimmed.json);
      env = {
        # TERM variable
        # This value is used to set the `$TERM` environment variable for
        # each instance of Alacritty. If it is not present, alacritty will
        # check the local terminfo database and use `alacritty` if it is
        # available, otherwise `xterm-256color` is used.
        TERM = "xterm-256color";
      };
      window = {
        opacity = 1.0;
        # Allow terminal applications to change Alacritty's window title.
        dynamic_title = true;
        # Window position (changes require restart)
        # Specified in number of pixels.
        # If the position is not set, the window manager will handle the placement.
        # position = {
        #   x = 0;
        #   y = 0;
        # };
        # Window padding (changes require restart)
        # Blank space added around the window in pixels. This padding is scaled
        # by DPI and the specified value is always added at both opposing sides.
        padding = {
          x = 10;
          y = 10;
        };
        # Spread additional padding evenly around the terminal content.
        dynamic_padding = true;
        # Window decorations
        # Values for `decorations`:
        #     - full: Borders and title bar
        #     - none: Neither borders nor title bar
        # Values for `decorations` (macOS only):
        #     - transparent: Title bar, transparent background and title bar buttons
        #     - buttonless: Title bar, transparent background, but no title bar buttons
        decorations = "buttonless";
        # Startup Mode (changes require restart)
        # Values for `startup_mode`:
        #   - Windowed
        #   - Maximized
        #   - Fullscreen
        # Values for `startup_mode` (macOS only):
        #   - SimpleFullscreen
        startup_mode = "Maximized";
      }; # window
      draw_bold_text_with_bright_colors = false;
      scrolling = {
        # Maximum number of lines in the scrollback buffer.
        # Specifying '0' will disable scrolling.
        history = 0;
      };
      font = {
        # Normal (roman) font face
        normal = {
          family = "PragmataPro Liga";
          style = "Regular";
        };
        bold = {
          family = "PragmataPro Liga";
          style = "Bold";
        };
        size = 21.0;
        # Offset is the extra space around each character. `offset.y` can be thought of
        # as modifying the line spacing, and `offset.x` as modifying the letter spacing.
        offset = {
          x = 0;
          y = 8;
        };
        # Glyph offset determines the locations of the glyphs within their cells with
        # the default being at the bottom. Increasing `x` moves the glyph to the right,
        # increasing `y` moves the glyph upwards.
        glyph_offset = {
          x = 0;
          y = 4;
        };
        use_thin_strokes = false;
      };
      bell = {
        animation = "EaseOutExpo";
        duration = 0;
        color = "0xffffff";
      };
      selection = {
        save_to_clipboard = true;
      };
      cursor = {
        unfocused_hollow = true;
      };
      mouse_bindings = [
        {
          mouse = "Middle";
          action = "PasteSelection";
        }
      ];
    }; # settings
  }; # alacritty
  programs.skim = {
    enable = true;
    enableZshIntegration = true;
  };
  programs.starship = {
    enable = true;
  };
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
      set -g default-terminal "xterm-24bit"
      set -ga terminal-overrides ",xterm-24bit:Tc"
      # set -g default-terminal "xterm-256color"
      # set -ga terminal-overrides ",alacritty:Tc"
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
      set -g status-right-length 60
      # set -g status-right "#[bg=#444455]#[fg=#bbbbcc] %H:%M "
      set -g status-right ""
      set -g default-terminal "xterm-24bit"
      set -g default-command "reattach-to-user-namespace -l zsh"
      set -g status-left "#[bg=#e63634]#[fg=brightwhite]#{?client_prefix,#[bg=green],} #S "
      set -g status-right "#[bg=#444444]#[fg=#888888] #(rainbarf --width 20 --rgb --no-battery --order fciaws)"
      bind-key y run "tmux save-buffer - | reattach-to-user-namespace pbcopy"
    '';
  };
  programs.zoxide = {
    enable = true;
  };
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
    envExtra = builtins.readFile ./.zshenv-mmbpm1;
    initExtra = ''
      export PATH="''${HOME}/.local/bin:''${PATH}:''${HOME}/.cargo/bin"
      printf '\e]2;'$(hostname)'\a'
    '';
  };
}
