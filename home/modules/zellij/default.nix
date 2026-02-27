{ pkgs, ... }: {
  home.packages = with pkgs; [ zellij ];
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
}
