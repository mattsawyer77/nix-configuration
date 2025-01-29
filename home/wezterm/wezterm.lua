---@diagnostic disable-next-line: undefined-global
local w = wezterm
---@diagnostic disable-next-line: unused-local
local favoriteSchemes = {
  ["iceberg-dark"] = true,
  ["Floraverse"] = true,
  ["Kanagawa (Gogh)"] = true,
  ["Horizon Dark (base16)"] = true,
  ["Ocean Dark (Gogh)"] = true,
  ["Mashup Colors (terminal.sexy)"] = true,
  ["Neon Night (Gogh)"] = true,
  ["Argonaut"] = true,
  ["nightfox"] = true,
  ["Codeschool (light) (terminal.sexy)"] = true,
  ["Hemisu Light"] = true,
  ["Spring"] = true,
  ["Papercolor Light (Gogh)"] = true,
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
      local scheme = schemesToSearch[i+1]
      if scheme == nil then
        break
      end
      found = true
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

local config = {
  ---@diagnostic disable-next-line: undefined-global
  font = w.font_with_fallback {
    -- { family = 'JetBrains Mono', weight = 'Thin' },
    -- { family = 'Victor Mono', weight = 'Thin' },
    'PragmataPro Liga',
    'Meslo',
    -- , {
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
  },
  font_size = 20.0,
  color_scheme = "Kanagawa (Gogh)",
  -- color_scheme = "Floraverse",
  -- color_scheme = "Horizon Dark (base16)",
  -- color_scheme = "Ocean Dark (Gogh)",
  -- color_scheme = "Mashup Colors (terminal.sexy)",
  -- color_scheme = "Neon Night (Gogh)",
  -- color_scheme = "Argonaut",
  -- color_scheme = "iceberg-dark",
  -- color_scheme = "nightfox",
  hide_tab_bar_if_only_one_tab = false,
  line_height = 1.1,
  freetype_load_flags = 'NO_HINTING',
  window_padding = {
    left = "10pt",
    right = "10pt",
    top = "1cell",
    bottom = "10pt",
  },
  window_decorations = "INTEGRATED_BUTTONS|RESIZE",
  keys = {
    {key="f", mods="CMD|CTRL", action="ToggleFullScreen"},
    -- Theme Cycler (favorite schemes only)
    { key = "t", mods = "ALT", action = w.action_callback(function(window) themeCycler(window, true) end) },
    -- Theme Cycler (all schemes)
    { key = "t", mods = "CMD|ALT", action = w.action_callback(function(window) themeCycler(window, false) end) },
    { key = "F9", action = w.action_callback(function(window) themeCycler(window, true) end) },

    -- Look up Scheme you switched to
    { key = "Escape", mods = "CTRL", action = w.action.ShowDebugOverlay },
  }
}

-- local gpus = w.gui.enumerate_gpus()
-- if #gpus > 0 then
--   config.webgpu_preferred_adapter = gpus[1]
-- seems WebGpu is required, make it unconditional for now
-- see https://github.com/wez/wezterm/issues/6005
  config.front_end = "WebGpu"
-- end
config.ssh_domains = {
  {
    -- multiplexing = 'None',
    multiplexing = 'WezTerm',
    -- The name of this specific domain.  Must be unique amongst
    -- all types of domain in the configuration file.
    name = 'haystack-ts',
    -- identifies the host:port pair of the remote server
    -- Can be a DNS name or an IP address with an optional
    -- ":port" on the end.
    remote_address = 'haystack-ts:22',
    -- Whether agent auth should be disabled.
    -- Set to true to disable it.
    -- no_agent_auth = false,
    -- The username to use for authenticating with the remote host
    username = 'sawyer',
    -- If true, connect to this domain automatically at startup
    -- connect_automatically = true,
    -- Specify an alternative read timeout
    -- timeout = 60,
    -- The path to the wezterm binary on the remote host.
    -- Primarily useful if it isn't installed in the $PATH
    -- that is configure for ssh.
    -- remote_wezterm_path = "/home/yourusername/bin/wezterm"
  },
  {
    multiplexing = 'WezTerm',
    name = 'haystack-f5',
    remote_address = 'haystack-f5:22',
    username = 'sawyer',
  },
}
config.unix_domains = {
  {
    name = "main",
  },
}
config.default_gui_startup_args = { 'connect', 'main' }
config.leader = { key = ' ', mods = 'CTRL', timeout_milliseconds = 1000 }
config.keys = {
  {
    mods   = "LEADER",
    key    = "\"",
    action = w.action.SplitVertical { domain = 'CurrentPaneDomain' }
  },
  {
    mods   = "LEADER",
    key    = "%",
    action = w.action.SplitHorizontal { domain = 'CurrentPaneDomain' }
  },
  -- {
  --   mods   = "LEADER",
  --   key    = "w",
  --   action = w.action.ShowTabNavigator
  -- }
  {
    mods   = "LEADER",
    key    = "w",
    action = wezterm.action.ShowTabNavigator,
  },
  {
    mods = "LEADER",
    key = "l",
    action = w.action.ActivateLastTab,
  },
  {
    mods = "LEADER",
    key = 'o',
    action = w.action.PaneSelect {
      mode = 'Activate',
      alphabet = '0123456789',
    },
  },
  {
    mods = "LEADER",
    key = 'O',
    action = w.action.PaneSelect {
      mode = 'SwapWithActive',
      alphabet = '0123456789',
    },
  },
  {
    mods = "LEADER",
    key = "z",
    action = w.action.TogglePaneZoomState
  },
}
-- pretty
config.window_frame = {
  font = w.font {
    family = 'JetBrains Mono',
    weight = 'Thin',
  },
  font_size = 14.0,
  active_titlebar_bg = '#161821',
  inactive_titlebar_bg = '#161821',
}
config.colors = {
  tab_bar = {
    inactive_tab_edge = '#575757',
  },
}

-- retro-style tab bar
-- config.tab_bar_at_bottom = true
-- config.colors = {
--   tab_bar = {
--     -- The color of the strip that goes along the top of the window
--     -- (does not apply when fancy tab bar is in use)
--     background = '#0b0022',

--     -- The active tab is the one that has focus in the window
--     active_tab = {
--       -- The color of the background area for the tab
--       bg_color = '#161821',
--       -- The color of the text for the tab
--       fg_color = '#c0c0c0',

--       -- Specify whether you want "Half", "Normal" or "Bold" intensity for the
--       -- label shown for this tab.
--       -- The default is "Normal"
--       intensity = 'Normal',

--       -- Specify whether you want "None", "Single" or "Double" underline for
--       -- label shown for this tab.
--       -- The default is "None"
--       underline = 'None',

--       -- Specify whether you want the text to be italic (true) or not (false)
--       -- for this tab.  The default is false.
--       italic = false,

--       -- Specify whether you want the text to be rendered with strikethrough (true)
--       -- or not for this tab.  The default is false.
--       strikethrough = false,
--     },

--     -- Inactive tabs are the tabs that do not have focus
--     inactive_tab = {
--       bg_color = '#1b1032',
--       fg_color = '#808080',

--       -- The same options that were listed under the `active_tab` section above
--       -- can also be used for `inactive_tab`.
--     },

--     -- You can configure some alternate styling when the mouse pointer
--     -- moves over inactive tabs
--     inactive_tab_hover = {
--       bg_color = '#3b3052',
--       fg_color = '#909090',
--       -- italic = true,

--       -- The same options that were listed under the `active_tab` section above
--       -- can also be used for `inactive_tab_hover`.
--     },

--     -- The new tab button that let you create new tabs
--     new_tab = {
--       bg_color = '#1b1032',
--       fg_color = '#808080',

--       -- The same options that were listed under the `active_tab` section above
--       -- can also be used for `new_tab`.
--     },

--     -- You can configure some alternate styling when the mouse pointer
--     -- moves over the new tab button
--     new_tab_hover = {
--       bg_color = '#3b3052',
--       fg_color = '#909090',
--       -- italic = true,

--       -- The same options that were listed under the `active_tab` section above
--       -- can also be used for `new_tab_hover`.
--     },
--   },
-- }

wezterm.on('update-right-status', function(window, pane)
  window:set_right_status(window:active_workspace())
end)

-- XXX broken
-- w.on('update-right-status', function(window, pane)
--   local session_name = window:mux_window().get_title();
--   -- local date = w.strftime '%Y-%m-%d %H:%M:%S'

--   -- Make it italic and underlined
--   window:set_right_status(w.format {
--     -- { Attribute = { Underline = 'Single' } },
--     -- { Attribute = { Italic = true } },
--     -- { Text = 'Hello ' .. date },
--     { Text = session_name }
--   })
-- end)
-- How many lines of scrollback you want to retain per tab
config.scrollback_lines = 10000
config.enable_scroll_bar = true

return config
