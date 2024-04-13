---@diagnostic disable-next-line: undefined-global
local w = wezterm
---@diagnostic disable-next-line: unused-local
local favoriteSchemes = {
  ["Floraverse"] = true,
  ["Kanagawa (Gogh)"] = true,
  ["Horizon Dark (base16)"] = true,
  ["Ocean Dark (Gogh)"] = true,
  ["OceanDark (Gogh)"] = true,
  ["Mashup Colors (terminal.sexy)"] = true,
  ["Neon Night (Gogh)"] = true,
  ["Argonaut"] = true,
  ["iceberg-dark"] = true,
  ["nightfox"] = true,
  ["Codeschool (light) (terminal.sexy)"] = true,
  ["Hemisu Light"] = true,
};

---cycle through builtin dark schemes in dark mode,
---and through light schemes in light mode
local function themeCycler(window, useFavorites, _)
  if useFavorites then
    w.log_info("starting themeCycler (useFavorites: true)...")
  else
    w.log_info("starting themeCycler (useFavorites: false)...")
  end
  local allSchemes = w.color.get_builtin_schemes()
  w.log_info("found builtin schemes")
  local currentMode = w.gui.get_appearance()
  if currentMode:find("Dark") then
  w.log_info("found appearance: dark")
  else
  w.log_info("found appearance: light")
  end
  local currentScheme = window:effective_config().color_scheme
  w.log_info("found currentScheme" .. currentScheme)
  local darkSchemes = {}
  local lightSchemes = {}

  for name, scheme in pairs(allSchemes) do
    if not useFavorites or favoriteSchemes[name] == true then
      local bg = {}
      local parsed = pcall(function()
        local maybe_bg = w.color.parse(scheme.background)
        bg = maybe_bg
      end) -- parse into a color object
      if parsed then
        ---@diagnostic disable-next-line: unused-local
        local h, s, l, a = bg:hsla() -- and extract HSLA information
        if l < 0.4 then
          w.log_info(" - adding scheme to darkSchemes: " .. name)
          table.insert(darkSchemes, name)
          w.log_info(" - scheme added to darkSchemes: " .. name)
        else
          w.log_info(" - adding scheme to lightSchemes: " .. name)
          table.insert(lightSchemes, name)
          w.log_info(" - scheme added to lightSchemes: " .. name)
        end
      end
    end
  end
  w.log_info("examined all schemes")
  local schemesToSearch = currentMode:find("Dark") and darkSchemes or lightSchemes

  local overrides = window:get_config_overrides() or {}
  local found = false
  for i = 1, #schemesToSearch, 1 do
    if schemesToSearch[i] == currentScheme then
      found = true
      local scheme = schemesToSearch[i+1]
      if scheme == nil then
        break
      end
      overrides.color_scheme = scheme
      window:set_config_overrides(overrides)
      w.log_info("Switched to: " .. scheme)
      return
    end
  end
  if not found then
    w.log_info("current scheme not found in favorites, switching to the first scheme")
    overrides.color_scheme = schemesToSearch[1]
    window:set_config_overrides(overrides)
    w.log_info("Switched to: " .. schemesToSearch[1])
  end
end
-- overrides = window:get_config_overrides(); overrides.color_scheme = "Floraverse"; window:set_config_overrides(overrides)
-- overrides = window:get_config_overrides(); overrides.color_scheme = "Kanagawa (Gogh)"; window:set_config_overrides(overrides)
-- overrides = window:get_config_overrides(); overrides.color_scheme = "Horizon Dark (base16)"; window:set_config_overrides(overrides)
-- overrides = window:get_config_overrides(); overrides.color_scheme = "Ocean Dark (Gogh)"; window:set_config_overrides(overrides)
-- overrides = window:get_config_overrides(); overrides.color_scheme = "OceanDark (Gogh)"; window:set_config_overrides(overrides)
-- overrides = window:get_config_overrides(); overrides.color_scheme = "Mashup Colors (terminal.sexy)"; window:set_config_overrides(overrides)
-- overrides = window:get_config_overrides(); overrides.color_scheme = "Neon Night (Gogh)"; window:set_config_overrides(overrides)
-- overrides = window:get_config_overrides(); overrides.color_scheme = "Argonaut"; window:set_config_overrides(overrides)
-- overrides = window:get_config_overrides(); overrides.color_scheme = "iceberg-dark"; window:set_config_overrides(overrides)
-- overrides = window:get_config_overrides(); overrides.color_scheme = "nightfox"; window:set_config_overrides(overrides)

return {
  ---@diagnostic disable-next-line: undefined-global
  font = w.font('PragmataPro Liga', {
                        -- weight = "Thin"
                        -- weight = "ExtraLight"
                        -- weight = "Light"
                        -- weight = "DemiLight"
                        -- weight = "Book"
                        -- weight = "Regular"
                        -- weight = "Medium"
                        -- weight = "DemiBold"
                        -- weight = "Bold"
                        -- weight = "ExtraBold"
                        -- weight = "Black"
                        -- weight = "ExtraBlack"
  }),
  font_size = 22.0,
  -- color_scheme = "Kanagawa (Gogh)",
  -- color_scheme = "Floraverse",
  -- color_scheme = "Horizon Dark (base16)",
  -- color_scheme = "Ocean Dark (Gogh)",
  -- color_scheme = "OceanDark (Gogh)",
  -- color_scheme = "Mashup Colors (terminal.sexy)",
  color_scheme = "Neon Night (Gogh)",
  -- color_scheme = "Argonaut",
  -- color_scheme = "iceberg-dark",
  -- color_scheme = "nightfox",
  hide_tab_bar_if_only_one_tab = true,
  line_height = 1.1,
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
    -- Theme Cycler (favorite schemes only)
    { key = "t", mods = "ALT", action = w.action_callback(function(window) themeCycler(window, true) end) },
    -- Theme Cycler (all schemes)
    { key = "t", mods = "CMD|ALT", action = w.action_callback(function(window) themeCycler(window, false) end) },
    { key = "f9", action = w.action_callback(function(window) themeCycler(window, true) end) },

    -- Look up Scheme you switched to
    { key = "Escape", mods = "CTRL", action = w.action.ShowDebugOverlay },
  }
}
