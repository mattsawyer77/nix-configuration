{ ... }:
let
  # map the following keys from ctrl to cmd
  ctrlToCMDKeys = [ "c" "v" "x" "f" "g" "t" "n" "z" ];
  karabinerConfig = {
    global = {
      check_for_updates_on_startup = false;
      show_in_menu_bar = true;
      show_profile_name_in_menu_bar = false;
    };
    profiles = [{
      complex_modifications = {
        parameters = {
          "basic.simultaneous_threshold_milliseconds" = 50;
          "basic.to_delayed_action_delay_milliseconds" = 500;
          "basic.to_if_alone_timeout_milliseconds" = 1000;
          "basic.to_if_held_down_threshold_milliseconds" = 500;
          "mouse_motion_to_scroll.speed" = 100;
        };
        rules = [
          {
            description = "map cmd variants to ctrl";
            manipulators = (map
              (key: {
                conditions = [{
                  type = "frontmost_application_unless";
                  bundle_identifiers = [
                    "^org\\.alacritty"
                    "^io\\.alacritty"
                    ".*wezterm.*"
                    "^org\\.gnu\\.emacs"
                    "^org\\.gnu\\.Emacs"
                    "com.jetbrains.goland"
                    "com.microsoft.VSCode"
                    "dev.zed.Zed"
                    "com.mitchellh.ghostty"
                  ];
                  file_paths = [
                    ".*/wezterm-gui"
                  ];
                }];
                from = {
                  modifiers = { mandatory = [ "control" ]; };
                  key_code = key;
                };
                to = {
                  modifiers = [ "command" ];
                  key_code = key;
                };
                type = "basic";
              })
              ctrlToCMDKeys);
          }
          {
            description = "alacritty: map cmd+0-9 keys to alt+0-9";
            manipulators = (map
              (key: {
                conditions = [{
                  bundle_identifiers = [
                    "^org\\.alacritty"
                    "^io\\.alacritty"
                  ];
                }];
                from = {
                  modifiers = { mandatory = [ "left_command" ]; };
                  key_code = key;
                };
                to = {
                  modifiers = [ "left_option" ];
                  key_code = key;
                };
                type = "basic";
              })
              (builtins.genList (x: builtins.toString x) 10)); # i.e. "0" thru "9"
          }
          {
            description = "firefox/zen customization";
            manipulators = [{
              conditions = [{
                bundle_identifiers = [
                  "^org\\.mozilla\\."
                  "^com\\.kagi\\.kagimacOS"
                  "^app\\.zen-browser\\.zen"
                ];
                type = "frontmost_application_if";
              }];
              description = "reload the page with ctrl+r";
              from = {
                key_code = "r";
                modifiers = { mandatory = [ "left_control" ]; };
              };
              to = [{
                key_code = "r";
                modifiers = [ "left_command" ];
              }];
              type = "basic";
            }];
          }
          {
            description = "Apple Mail customization";
            manipulators = [{
              conditions = [{
                bundle_identifiers = [ "^com\\.apple\\.mail" ];
                type = "frontmost_application_if";
              }];
              description = "send mail with cmd+enter";
              from = {
                key_code = "return_or_enter";
                modifiers = { mandatory = [ "command" ]; };
              };
              to = [{
                key_code = "d";
                modifiers = [ "shift" "command" ];
              }];
              type = "basic";
            }];
          }
          {
            description = "Post Esc if left Control is tapped, Control if held.";
            manipulators = [{
              conditions = [{
                bundle_identifiers = [ "^org\\.mozilla\\." ];
                type = "frontmost_application_unless";
              }];
              from = {
                key_code = "left_control";
                modifiers = { optional = [ "any" ]; };
              };
              to = [{
                key_code = "left_control";
                lazy = true;
              }];
              to_if_alone = [{ key_code = "escape"; }];
              type = "basic";
            }];
          }
          {
            description = "Post Esc if Caps is tapped, Control if held.";
            manipulators = [{
              from = {
                key_code = "caps_lock";
                modifiers = { optional = [ "any" ]; };
              };
              to = [{
                key_code = "left_control";
                lazy = true;
              }];
              to_if_alone = [{ key_code = "escape"; }];
              type = "basic";
            }];
          }
          {
            description =
              "Control+Command+Backspace to Lock Screen (macOS 10.13+)";
            manipulators = [{
              from = {
                key_code = "delete_or_backspace";
                modifiers = { mandatory = [ "control" "command" ]; };
              };
              to = [{
                key_code = "q";
                modifiers = [ "right_control" "right_command" ];
              }];
              type = "basic";
            }];
          }
          {
            description = "fix emacs full-screen";
            manipulators = [{
              conditions = [{
                bundle_identifiers = [ "^org\\.gnu\\.emacs" ];
                type = "frontmost_application_if";
              }];
              from = {
                key_code = "f";
                modifiers = { mandatory = [ "command" "left_control" ]; };
              };
              to = [{
                shell_command =
                  "osascript -e 'activate application id \"org.gnu.emacs\"' -e 'tell application \"System Events\" to tell process \"Emacs\"' -e 'set emacsWindow to (first item of (every UI element whose subrole is \"AXStandardWindow\"))' -e 'if value of attribute \"AXFullScreen\" of emacsWindow is false then' -e 'set value of attribute \"AXFullScreen\" of emacsWindow to true' -e 'else' -e 'set value of attribute \"AXFullScreen\" of emacsWindow to false' -e 'end if' -e 'end tell'";
              }];
              type = "basic";
            }];
          }
          {
            description =
              "mute/unmute audio input with pause key (or Keychron Q1 knob)";
            manipulators = [{
              from = { key_code = "pause"; };
              to = [{
                # shell function defined in home/.zshenv-KD21QWDKW7.nix
                shell_command =
                  "/etc/profiles/per-user/$USER/bin/zsh -c toggle-audio-input-mute";
                # previous attempt at just muting/unmuting zoom (works but is specific to zoom):
                # shell_command = "osascript -e 'tell application \"System Events\"' -e 'activate application id \"us.zoom.xos\"' -e 'keystroke \"a\" using {command down, shift down}' -e 'end tell'";
              }];
              type = "basic";
            }];
          }
          # XXX: the following doesn't work anymore, have to edit the option in Firefox about:config
          # {
          #   description = "Safari customization";
          #   manipulators = [{
          #     conditions = [{
          #       bundle_identifiers = [ "^org\\.apple\\.safari" ];
          #       type = "frontmost_application_if";
          #     }];
          #     description = "prevent esc from from exiting full screen";
          #     from = { key_code = "escape"; };
          #     to = [{
          #       key_code = "escape";
          #       modifiers = [ "shift" ];
          #     }];
          #     type = "basic";
          #   }];
          # }

          {
            description =
              "map ctrl+space to tab search in Firefox/Zen";
            manipulators = [{
              conditions = [{
                type = "frontmost_application_if";
                bundle_identifiers = [
                  "^org\\.mozilla\\."
                  "^app\\.zen-browser\\.zen"
                ];
              }];
              from = {
                modifiers = { mandatory = [ "left_control" ]; };
                key_code = "spacebar";
              };
              to = [
                {
                  modifiers = [ "left_command" ];
                  key_code = "l";
                }
                {
                  # typing % followed by a space in the search bar activates "tab search"
                  modifiers = [ "right_shift" ];
                  key_code = "5";
                }
                {
                  modifiers = [ ];
                  key_code = "spacebar";
                  hold_down_milliseconds = 30;
                }
              ];
              type = "basic";
            }];
          }
          {
            description =
              "map cmd+y to history sidebar in Firefox";
            manipulators = [{
              conditions = [{
                type = "frontmost_application_if";
                bundle_identifiers = [
                  "^org\\.mozilla\\."
                  "^app\\.zen-browser\\.zen"
                ];
              }];
              from = {
                modifiers = { mandatory = [ "left_command" ]; };
                key_code = "y";
              };
              to = [
                {
                  modifiers = [ "right_shift" "right_command" ];
                  key_code = "h";
                }
              ];
              type = "basic";
            }];
          }

          {
            description =
              "map cmd+shift+y to history window in Firefox";
            manipulators = [{
              conditions = [{
                type = "frontmost_application_if";
                bundle_identifiers = [
                  "^org\\.mozilla\\."
                  "^app\\.zen-browser\\.zen"
                ];
              }];
              from = {
                modifiers = [ "left_shift" "left_command" ];
                key_code = "h";
              };
              to = [
                {
                  modifiers = [ "right_command" ];
                  key_code = "y";
                }
              ];
              type = "basic";
            }];
          }
        ];
      };
      devices = [
        {
          disable_built_in_keyboard_if_exists = false;
          identifiers = {
            is_keyboard = true;
            is_pointing_device = false;
            product_id = 402;
            vendor_id = 1241;
          };
          ignore = false;
          manipulate_caps_lock_led = false;
          simple_modifications = [
            {
              from = { key_code = "left_command"; };
              to = [{ key_code = "left_option"; }];
            }
            {
              from = { key_code = "left_option"; };
              to = [{ key_code = "left_command"; }];
            }
            {
              from = { key_code = "right_option"; };
              to = [{ key_code = "right_command"; }];
            }
          ];
        }
        {
          disable_built_in_keyboard_if_exists = false;
          fn_function_keys = [ ];
          identifiers = {
            is_keyboard = true;
            is_pointing_device = false;
            product_id = 34304;
            vendor_id = 1452;
          };
          ignore = false;
          manipulate_caps_lock_led = true;
        }
        {
          disable_built_in_keyboard_if_exists = false;
          # fn_function_keys = [ ];
          identifiers = {
            is_keyboard = true;
            is_pointing_device = false;
            product_id = 263;
            vendor_id = 13364;
          };
          ignore = false;
          ignore_vendor_events = true;
          manipulate_caps_lock_led = true;
          # seems that the "pointing device" keychron q1 is what matters here?
          # simple_modifications = functionKeySimpleMods;
        }
        {
          disable_built_in_keyboard_if_exists = false;
          identifiers = {
            is_keyboard = true;
            is_pointing_device = true;
            product_id = 263;
            vendor_id = 13364;
          };
          ignore = false;
          ignore_vendor_events = true;
          manipulate_caps_lock_led = true;
          # seems that the "pointing device" keychron q1 is what matters here?
          # fn_function_keys = fnFunctionKeys;
          # simple_modifications = [];
        }
        {
          disable_built_in_keyboard_if_exists = false;
          # fn_function_keys = fnFunctionKeys;
          identifiers = {
            is_keyboard = true;
            is_pointing_device = false;
            product_id = 636;
            vendor_id = 1452;
          };
          ignore = false;
          manipulate_caps_lock_led = true;
        }
        {
          disable_built_in_keyboard_if_exists = false;
          fn_function_keys = [ ];
          identifiers = {
            is_keyboard = false;
            is_pointing_device = true;
            product_id = 636;
            vendor_id = 1452;
          };
          ignore = true;
          manipulate_caps_lock_led = false;
          simple_modifications = [ ];
        }
        {
          disable_built_in_keyboard_if_exists = false;
          fn_function_keys = [ ];
          identifiers = {
            is_keyboard = true;
            is_pointing_device = false;
            product_id = 61138;
            vendor_id = 1240;
          };
          ignore = false;
          manipulate_caps_lock_led = true;
          simple_modifications = [
            {
              from = { key_code = "left_command"; };
              to = [{ key_code = "left_option"; }];
            }
            {
              from = { key_code = "left_option"; };
              to = [{ key_code = "left_command"; }];
            }
            {
              from = { key_code = "right_option"; };
              to = [{ key_code = "right_command"; }];
            }
          ];
        }
        {
          disable_built_in_keyboard_if_exists = false;
          fn_function_keys = [ ];
          identifiers = {
            is_keyboard = false;
            is_pointing_device = true;
            product_id = 61138;
            vendor_id = 1240;
          };
          ignore = false;
          manipulate_caps_lock_led = false;
          simple_modifications = [ ];
        }
        {
          disable_built_in_keyboard_if_exists = false;
          fn_function_keys = [ ];
          identifiers = {
            is_keyboard = false;
            is_pointing_device = true;
            product_id = 613;
            vendor_id = 76;
          };
          ignore = true;
          manipulate_caps_lock_led = false;
          simple_modifications = [ ];
        }
      ];
      name = "Default profile";
      parameters = { delay_milliseconds_before_open_device = 1000; };
      selected = true;
      simple_modifications = [
        {
          # toggle between light/dark mode
          from = { key_code = "f9"; };
          to = [{ shell_command = "osascript -e 'tell app \"System Events\" to tell appearance preferences to set dark mode to not dark mode'"; }];
        }
      ];
      virtual_hid_keyboard = {
        country_code = 0;
        indicate_sticky_modifier_keys_state = true;
        mouse_key_xy_scale = 100;
        keyboard_type_v2 = "ansi";
      };
    }];
  };
in
{
  # TODO: consider whether to install karabiner-elements package from nix
  # home.packages = [ pkgs.karabiner-elements ];
  home.file."karabiner.json" = {
    text = builtins.toJSON karabinerConfig;
    target = ".config/karabiner/karabiner.json";
  };
}
