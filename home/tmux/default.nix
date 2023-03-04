{ pkgs, lib, ... }:

let
  optionFlags = {
    "global" = "g";
    "pane" = "p";
    "window" = "w";
    "server" = "s";
    "append" = "a";
  };

  # utility function to map attrset to tmux option syntax
  # where flags is a list of strings, each element an option flag contained in the above list
  mapOptions = with builtins;
    (options:
      (
        let
          mapFlags = (flags:
            "-" + (builtins.concatStringsSep ""
              (builtins.map (flagName: optionFlags.${flagName}) flags)));
        in
        (concatStringsSep "\n" (map
          (option:
            "set-option ${mapFlags option.flags} ${option.name} ${option.value}")
          options))
      ));

  extraConfigOptions = [
    {
      name = "automatic-rename";
      value = "off";
      flags = [ "global" ];
    }
    {
      name = "focus-events";
      value = "on";
      flags = [ "global" ];
    }
    {
      name = "-q";
      value = "mode-mouse on";
      flags = [ "global" ];
    }
    {
      name = "-q";
      value = "mouse-resize-pane on";
      flags = [ "global" ];
    }
    {
      name = "-q";
      value = "mouse on";
      flags = [ "global" ];
    }
    {
      name = "history-limit";
      value = "50000";
      flags = [ "global" ];
    }
    {
      name = "escape-time";
      value = "10";
      flags = [ "global" ];
    }
    {
      name = "pane-border-style";
      value = "fg=colour235,bg=black";
      flags = [ "global" ];
    }
    {
      name = "pane-active-border-style";
      value = "fg=colour240,bg=black";
      flags = [ "global" ];
    }
    {
      name = "status";
      value = "on";
      flags = [ "global" ];
    }
    {
      # set update frequencey (default 15 seconds)
      name = "status-interval";
      value = "30";
      flags = [ "global" ];
    }
    {
      # center window list for clarity
      name = "status-justify";
      value = "left";
      flags = [ "global" ];
    }
    {
      name = "message-style";
      value = "bg=black,fg=green";
      flags = [ "global" ];
    }
    {
      name = "status-style";
      value = ''bg="#2c2c34",fg=yellow'';
      flags = [ "global" ];
    }
    {
      name = "window-status-format";
      value = ''"  #{window_index}|#{window_name}  "'';
      flags = [ "global" ];
    }
    {
      name = "window-status-style";
      value = ''fg="#888899",bg="#383845"'';
      flags = [ "global" ];
    }
    {
      name = "window-status-last-style";
      value = ''fg="#888899",bg="#383845"'';
      flags = [ "global" ];
    }
    {
      name = "window-status-current-format";
      value = ''"  #{window_index}|#{window_name}  "'';
      flags = [ "global" ];
    }
    {
      name = "window-status-current-style";
      value = ''fg="#ccccdd",bg="#4f4f58"'';
      flags = [ "global" ];
    }
    {
      name = "status-left-length";
      value = "70";
      flags = [ "global" ];
    }
    {
      name = "status-left";
      value = ''
        "#[bg=#336688]#[fg=#eeeeee] #h #[bg=#113355]#[fg=brightwhite]#{?client_prefix,#[bg=green],} #S "
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
      name = "default-command";
      value = ''"reattach-to-user-namespace -l zsh"'';
      flags = [ "global" ];
    }
    {
      name = "status-left";
      value = ''
        "#[bg=#e63634]#[fg=brightwhite]#{?client_prefix,#[bg=green],} #S "
      '';
      flags = [ "global" ];
    }
    {
      name = "default-terminal";
      value = "alacritty";
      flags = [ "global" ];
    }
    {
      # 24-bit color
      name = "terminal-overrides";
      value = ",alacritty:RGB";
      flags = [ "global" "append" ];
    }
  ];

  # utility function to map attrset to tmux key bind syntax
  # where command is something like "bind -n" or "bind"
  mapKeyBinds = with builtins;
    (command: keys:
      (concatStringsSep "\n" (attrValues
        (mapAttrs (key: value: "${command} ${key} ${value}") keys))));

  mapKeyUnbinds = with builtins;
    (keys: (concatStringsSep "\n" (map (key: "unbind ${key}") keys)));

  # default keys to unbind
  unbindKeys = [ "C-b" ];

  # keys to bind without prefix
  rootKeys = {
    "S-Up" = "set-option -g status on";
    "S-Down" = "set-option -g status off";
    "S-Left" = "previous-window";
    "S-Right" = "next-window";
    "f3" = "next-layout";
    "M-J" = "resize-pane -D 5";
    "M-K" = "resize-pane -U 5";
    "M-H" = "resize-pane -L 5";
    "M-L" = "resize-pane -R 5";
    "M-Left" = "select-pane -L";
    "M-Right" = "select-pane -R";
    "M-Up" = "select-pane -U";
    "M-Down" = "select-pane -D";
    "WheelUpPane" = ''
      if-shell -F -t = "#{mouse_any_flag}" "send-keys -M" "if -Ft= '#{pane_in_mode}' 'send-keys -M' 'copy-mode -e'"
    '';
  };

  # keys to bind with prefix
  prefixKeys = {
    "j" = ''
      command-prompt -p "join pane from:"  "join-pane -hs '%%'"
    '';
    "s" = "choose-tree";
    "b" = "break-pane";
    "BSpace" = ''
      send-keys " clear && tmux clear-history" \; send-keys "Enter"
    '';
    "c" = ''
      command-prompt -p "new window name:" "new-window -n '%%'"
    '';
    "y" = ''
      run "tmux save-buffer - | reattach-to-user-namespace pbcopy"
    '';
  };

  # keys to bind in vi copy-mode table
  viModeKeys = {
    "v" = "send -X begin-selection";
    "y" = "send -X copy-selection-and-cancel";
  };

in
{
  enable = true;
  package = pkgs.tmux;
  baseIndex = 1;
  clock24 = true;
  disableConfirmationPrompt = true;
  historyLimit = 50000;
  keyMode = "vi";
  mouse = true;
  prefix = "C-space";
  shortcut = "space"; # ??
  sensibleOnTop = false;
  shell = "${pkgs.zsh}/bin/zsh";
  extraConfig = ''
    ${mapKeyUnbinds unbindKeys}
    ${mapKeyBinds "bind -n" rootKeys}
    ${mapKeyBinds "bind" prefixKeys}
    ${mapKeyBinds "bind-key -T copy-mode-vi" viModeKeys}
    ${mapOptions extraConfigOptions}
  '';
}
