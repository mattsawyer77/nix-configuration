{ config, pkgs, ... }:

let
  username = "sawyer";
  homeDirectory = "/Users/" + username;
  goPathSuffix = "gocode";

in
{
  home = {
    homeDirectory = homeDirectory;
    packages = [ ];
    stateVersion = "22.11";
    username = username;
    # append these extra dirs to the nix-generated path
    sessionPath = [
      (homeDirectory + "/.local/bin")
      (homeDirectory + "/.cargo/bin")
      (homeDirectory + "/" + goPathSuffix + "/bin")
    ];
    sessionVariables = {
      COLORTERM = "truecolor";
      EDITOR = "em";
      VISUAL = "em";
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
        # TERM = "xterm-256color";
        TERM = "alacritty";
      };
      key_bindings = [
        # map ctrl+space to ctrl+l since zellij doesn't support ctrl+space
        {
          key = "Space";
          mods = "Control";
          chars = "\\x0c";
        }
      ]; # key_bindings
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
        decorations = "full";
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
          # family = "JetBrains Mono";
          # style = "Thin";
          family = "PragmataPro Liga";
          style = "Regular";
        };
        bold = {
          family = "PragmataPro Liga";
          style = "Bold";
        };
        size = 22.0;
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
        AppleFontSmoothing = false;
      }; # font
      bell = {
        animation = "EaseOutExpo";
        duration = 0;
        color = "0xffffff";
      };
      selection = { save_to_clipboard = true; };
      cursor = { unfocused_hollow = true; };
      mouse_bindings = [{
        mouse = "Middle";
        action = "PasteSelection";
      }];
    }; # settings
  }; # alacritty
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
  };
  programs.starship = { enable = true; };
  programs.tmux = {
    enable = true;
    extraConfig = ''
      # remap prefix to control+l
      unbind C-b
      set -g prefix C-l
      # set -g prefix C-space
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
      set -g status-right-length 60
      # set -g status-right "#[bg=#444455]#[fg=#bbbbcc] %H:%M "
      set -g status-right ""
      set -g default-command "reattach-to-user-namespace -l zsh"
      set -g status-left "#[bg=#e63634]#[fg=brightwhite]#{?client_prefix,#[bg=green],} #S "
      set -g status-right "#[bg=#444444]#[fg=#888888] #(rainbarf --width 20 --rgb --no-battery --order fciaws)"
      bind-key y run "tmux save-buffer - | reattach-to-user-namespace pbcopy"
    '';
  };
  programs.zellij = {
    enable = true;
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
      l = "exa -alF";
      ts = "tmux new-session -n main -s";
      ta = "tmux attach -t";
      tl = "tmux list-sessions";
      em = "em.zsh";
      doom = "~/.emacs.d/bin/doom";
      zs = "zellij --layout compact --session";
      za = "zellij attach";
      zl = "zellij list-sessions";
    };
    envExtra = builtins.readFile ./.zshenv-SEA-ML-00059144;
    initExtra = ''
      source <(kubectl completion zsh)
      printf '\e]2;'$(hostname)'\a'
    '';
  };
}
