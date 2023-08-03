{ fontConfig, ... }:
let
  alacrittyThemes = {
    afterglow = builtins.fromJSON (builtins.readFile ./themes/Afterglow.json);
    argonaut = builtins.fromJSON (builtins.readFile ./themes/Argonaut.json);
    atelierLakeside = builtins.fromJSON (builtins.readFile ./themes/Atelierlakeside.dark.json);
    ayuDark = builtins.fromJSON (builtins.readFile ./themes/Ayu-Dark.json);
    ayuMirage = builtins.fromJSON (builtins.readFile ./themes/Ayu-Mirage.json);
    brewer = builtins.fromJSON (builtins.readFile ./themes/Brewer.dark.json);
    eqie6 = builtins.fromJSON (builtins.readFile ./themes/Eqie6.json);
    hybrid = builtins.fromJSON (builtins.readFile ./themes/Hybrid.json);
    icebergDark = builtins.fromJSON (builtins.readFile ./themes/Iceberg-Dark.json);
    oceanDark = builtins.fromJSON (builtins.readFile ./themes/Ocean.dark.json);
    paleNight = builtins.fromJSON (builtins.readFile ./themes/Palenight.json);
    tokyoNight = builtins.fromJSON (builtins.readFile ./themes/Tokyonight_Night.json);
    twilightDark = builtins.fromJSON (builtins.readFile ./themes/Twilight.dark.json);
    githubDimmed = builtins.fromJSON (builtins.readFile ./themes/github_dimmed.json);
  };
in
{
  enable = true;
  settings = {
    live_config_reload = true;
    colors = alacrittyThemes.eqie6;
    env = {
      # TERM = "xterm-256color";
      TERM = "alacritty";
    };
    key_bindings = [
      # map ctrl+space to ctrl+l since zellij doesn't support ctrl+space
      # {
      #   key = "Space";
      #  mods = "Control";
      #  chars = "\\x0c";
      #}
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
        # family = "Iosevka Extended";
        family = fontConfig.monospaceFamily;
        style = "Regular";
      };
      bold = {
        family = fontConfig.monospaceFamily;
        # family = "Iosevka Extended";
        # family = "Input";
        style = "Bold";
        # style = "Medium";
      };
      size = 23.0;
      # Offset is the extra space around each character. `offset.y` can be thought of
      # as modifying the line spacing, and `offset.x` as modifying the letter spacing.
      offset = {
        x = 0;
        y = 6;
      };
      # Glyph offset determines the locations of the glyphs within their cells with
      # the default being at the bottom. Increasing `x` moves the glyph to the right,
      # increasing `y` moves the glyph upwards.
      glyph_offset = {
        x = 0;
        y = 4;
      };
      AppleFontSmoothing = true;
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
}
