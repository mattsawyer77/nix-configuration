{ ... }:
{
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
      g = {
        b = "buffer_picker";
        i = "goto_last_change";
        I = "goto_implementation";
        t = "goto_type_definition";
      };
      G = "goto_file_end";
      P = "paste_clipboard_before";
      p = "paste_clipboard_after";
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
    edge = (builtins.fromJSON (builtins.readFile ./themes/edge.json));
    everforest = (builtins.fromJSON (builtins.readFile ./themes/everforest.json));
    gruvbox = (builtins.fromJSON (builtins.readFile ./themes/gruvbox.json));
    mogster = (builtins.fromJSON (builtins.readFile ./themes/mogster.json));
    sonokai = (builtins.fromJSON (builtins.readFile ./themes/sonokai.json));
  }; # themes
}
