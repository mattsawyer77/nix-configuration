{ config, lib, pkgs, ... }:

with lib; {
  programs.tmux.extraConfig = ''
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
  '' + (if pkgs.stdenv.isDarwin then ''
    set -g default-terminal "xterm-24bit"
    set -g default-command "reattach-to-user-namespace -l zsh"
    set -g status-left "#[bg=#e63634]#[fg=brightwhite]#{?client_prefix,#[bg=green],} #S "
    set -g status-right "#[bg=#444444]#[fg=#888888] #(rainbarf --width 20 --rgb --no-battery --order fciaws)"
    bind-key y run "tmux save-buffer - | reattach-to-user-namespace pbcopy"
  '' else ''
    # slightly different colors for linux
    set -g window-style 'fg=#a6d0e2,bg=#242f3b'
  '');
}
