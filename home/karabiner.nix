# map the following keys from ctrl to cmd
let ctrlToCMDKeys = [ "c" "v" "x" "f" "g" "t" "n" "z" ];
in
{
  global = {
    check_for_updates_on_startup = true;
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
                  "^org\\.gnu\\.emacs"
                  "^org\\.gnu\\.Emacs"
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
          description = "firefox customization";
          manipulators = [
            {
              conditions = [{
                bundle_identifiers = [ "^org\\.mozilla\\." ];
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
            }
            {
              conditions = [{
                bundle_identifiers = [ "^org\\.mozilla\\." ];
                type = "frontmost_application_if";
              }];
              description = "prevent esc from from exiting full screen";
              from = { key_code = "escape"; };
              to = [{
                key_code = "escape";
                modifiers = [ "shift" ];
              }];
              type = "basic";
            }
            {
              conditions = [{
                bundle_identifiers = [ "^org\\.mozilla\\." ];
                type = "frontmost_application_if";
              }];
              description = "map left ctrl to shift+esc if tapped";
              from = {
                key_code = "left_control";
                modifiers = { optional = [ "any" ]; };
              };
              to = [{
                key_code = "left_control";
                lazy = true;
              }];
              to_if_alone = [{
                key_code = "escape";
                modifiers = [ "shift" ];
              }];
              type = "basic";
            }
          ];
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
          description = "Post Esc if Caps is tapped, Control if held.";
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
          description = "Safari customization";
          manipulators = [{
            conditions = [{
              bundle_identifiers = [ "^org\\.apple\\.safari" ];
              type = "frontmost_application_if";
            }];
            description = "prevent esc from from exiting full screen";
            from = { key_code = "escape"; };
            to = [{
              key_code = "escape";
              modifiers = [ "shift" ];
            }];
            type = "basic";
          }];
        }
      ];
    };
    devices = [
      {
        disable_built_in_keyboard_if_exists = false;
        fn_function_keys = [
          {
            from = { key_code = "f1"; };
            to = [{ consumer_key_code = "mute"; }];
          }
          {
            from = { key_code = "f2"; };
            to = [{ consumer_key_code = "volume_decrement"; }];
          }
          {
            from = { key_code = "f3"; };
            to = [{ consumer_key_code = "volume_increment"; }];
          }
          {
            from = { key_code = "f4"; };
            to = [{ consumer_key_code = "rewind"; }];
          }
          {
            from = { key_code = "f5"; };
            to = [{ consumer_key_code = "play_or_pause"; }];
          }
          {
            from = { key_code = "f6"; };
            to = [{ consumer_key_code = "fastforward"; }];
          }
        ];
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
            from = { key_code = "caps_lock"; };
            to = [{ key_code = "left_control"; }];
          }
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
        simple_modifications = [ ];
      }
      {
        disable_built_in_keyboard_if_exists = false;
        fn_function_keys = [ ];
        identifiers = {
          is_keyboard = true;
          is_pointing_device = false;
          product_id = 263;
          vendor_id = 13364;
        };
        ignore = false;
        manipulate_caps_lock_led = true;
        simple_modifications = [{
          from = { key_code = "caps_lock"; };
          to = [{ key_code = "left_control"; }];
        }];
      }
      {
        disable_built_in_keyboard_if_exists = false;
        fn_function_keys = [ ];
        identifiers = {
          is_keyboard = true;
          is_pointing_device = true;
          product_id = 263;
          vendor_id = 13364;
        };
        ignore = false;
        manipulate_caps_lock_led = true;
        simple_modifications = [ ];
      }
      {
        disable_built_in_keyboard_if_exists = false;
        fn_function_keys = [ ];
        identifiers = {
          is_keyboard = true;
          is_pointing_device = false;
          product_id = 636;
          vendor_id = 1452;
        };
        ignore = false;
        manipulate_caps_lock_led = true;
        simple_modifications = [ ];
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
    fn_function_keys = [
      {
        from = { key_code = "f1"; };
        to = [{ consumer_key_code = "display_brightness_decrement"; }];
      }
      {
        from = { key_code = "f2"; };
        to = [{ consumer_key_code = "display_brightness_increment"; }];
      }
      {
        from = { key_code = "f3"; };
        to = [{ key_code = "mission_control"; }];
      }
      {
        from = { key_code = "f4"; };
        to = [{ key_code = "launchpad"; }];
      }
      {
        from = { key_code = "f5"; };
        to = [{ key_code = "illumination_decrement"; }];
      }
      {
        from = { key_code = "f6"; };
        to = [{ key_code = "illumination_increment"; }];
      }
      {
        from = { key_code = "f7"; };
        to = [{ consumer_key_code = "rewind"; }];
      }
      {
        from = { key_code = "f8"; };
        to = [{ consumer_key_code = "play_or_pause"; }];
      }
      {
        from = { key_code = "f9"; };
        to = [{ consumer_key_code = "fastforward"; }];
      }
      {
        from = { key_code = "f10"; };
        to = [{ consumer_key_code = "mute"; }];
      }
      {
        from = { key_code = "f11"; };
        to = [{ consumer_key_code = "volume_decrement"; }];
      }
      {
        from = { key_code = "f12"; };
        to = [{ consumer_key_code = "volume_increment"; }];
      }
    ];
    name = "Default profile";
    parameters = { delay_milliseconds_before_open_device = 1000; };
    selected = true;
    simple_modifications = [{
      from = { key_code = "caps_lock"; };
      to = [{ key_code = "left_control"; }];
    }];
    virtual_hid_keyboard = {
      country_code = 0;
      indicate_sticky_modifier_keys_state = true;
      mouse_key_xy_scale = 100;
    };
  }];
}
