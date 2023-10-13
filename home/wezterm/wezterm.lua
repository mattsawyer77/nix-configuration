return {
  font = wezterm.font("PragmataPro Liga"),
  font_size = 20.0,
  color_scheme = "Kanagawa (Gogh)",
  hide_tab_bar_if_only_one_tab = true,
  line_height = 1.2,
  freetype_load_flags = 'NO_HINTING',
  window_padding = {
    left = "5pt",
    right = "5pt",
    top = "1cell",
    bottom = "5pt",
  },
  window_decorations = "INTEGRATED_BUTTONS|RESIZE",
  keys = {
    {key="f", mods="CMD|CTRL", action="ToggleFullScreen"},
  }
}
