-- Hammerspoon Configuration
-- Left CTRL acts as ESC when tapped, CTRL when held
-- Firefox-specific: CTRL+space -> CMD+l, %, space

-- Track left CTRL key state
local controlPressed = false
local controlUsedWithOtherKey = false
-- map the following keys from ctrl to cmd
local ctrlToCMDKeys = {
  [8]  = "c",
  [9]  = "v",
  [7]  = "x",
  [3]  = "f",
  [5]  = "g",
  [17] = "t",
  [45] = "n",
  [6] = "z"
}
local ctrlToCMDKeysExcludeApps = {
  Alacritty = "Alacritty",
  Emacs = "Emacs",
  Zed = "Zed",
  Ghostty = "Ghostty"
}

-- Left CTRL key event handler
controlKeyHandler = hs.eventtap.new({hs.eventtap.event.types.flagsChanged, hs.eventtap.event.types.keyDown}, function(event)
    local flags = event:getFlags()
    local keyCode = event:getKeyCode()
    
    -- Check if left control key (keyCode 59)
    if keyCode == 59 or keyCode == 62 then
      -- print("control key pressed (or released)")
      if event:getType() == hs.eventtap.event.types.flagsChanged then
        if flags.ctrl then
          -- Left CTRL pressed
          controlPressed = true
          controlUsedWithOtherKey = false
        else
          -- Left CTRL released
          if controlPressed and not controlUsedWithOtherKey then
            -- Tap ESC
            -- print("sending escape")
            hs.eventtap.keyStroke({}, "escape")
          end
          controlPressed = false
          controlUsedWithOtherKey = false
        end
      end
      -- Don't suppress the original control event
      return false
    end
    
    -- Check for CTRL+space in Firefox
    if event:getType() == hs.eventtap.event.types.keyDown then
      if flags.ctrl and keyCode ~= 59 and keyCode ~= 62 then
        -- CTRL pressed with another key
        controlUsedWithOtherKey = true
        local app = hs.application.frontmostApplication()
        local appName = app:name()
        if appName == "Firefox" then
          -- Space bar keyCode is 49
          if keyCode == 49 then
            -- Send CMD+l (focus address bar)
            hs.eventtap.keyStroke({"cmd"}, "l")
            hs.timer.usleep(50000) -- 50ms delay
            
            -- Send %
            hs.eventtap.keyStrokes("%")
            hs.timer.usleep(50000) -- 50ms delay
            
            -- Send space
            hs.eventtap.keyStroke({}, "space")
            
            -- Suppress the original CTRL+space event
            return true
          elseif ctrlToCMDKeys[keyCode] then
            -- print("mapping ctrl->cmd for app/keycode ", appName, ctrlToCMDKeys[keyCode])
            hs.eventtap.keyStroke({"cmd"}, ctrlToCMDKeys[keyCode])
            return true
          end
          -- end of Firefox
        elseif ctrlToCMDKeysExcludeApps[appName] then
          -- print("not mapping ctrl->cmd for app", appName)
          return false
        elseif ctrlToCMDKeys[keyCode] then
          -- print("mapping ctrl->cmd for app/keycode ", appName, ctrlToCMDKeys[keyCode])
          hs.eventtap.keyStroke({"cmd"}, ctrlToCMDKeys[keyCode])
          return true
        else
          -- pass through original event
          return false
        end
      end
    end
    
    -- If any other key is pressed while control is held
    if controlPressed and event:getType() == hs.eventtap.event.types.keyDown then
      controlUsedWithOtherKey = true
    end
    
    return false
end)

controlKeyHandler:start()

-- Monitor eventtap health and restart if needed
local function checkEventTapHealth()
  if controlKeyHandler and not controlKeyHandler:isEnabled() then
    print("EventTap was disabled, attempting to restart...")
    controlKeyHandler:start()
    hs.notify.new({title="Hammerspoon", informativeText="EventTap restarted"}):send()
  end
end

-- Check every 5 seconds
healthTimer = hs.timer.doEvery(5, checkEventTapHealth)

-- Watch for sleep/wake events and restart eventtap
local function caffeinateWatcher(eventType)
  if eventType == hs.caffeinate.watcher.systemDidWake then
    print("System woke from sleep, restarting eventtap...")
    controlKeyHandler:stop()
    hs.timer.doAfter(1, function()
                       controlKeyHandler:start()
    end)
  end
end

sleepWatcher = hs.caffeinate.watcher.new(caffeinateWatcher)
sleepWatcher:start()

-- Manual restart hotkey (CMD+ALT+CTRL+R)
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "R", function()
    print("Manually restarting eventtap...")
    controlKeyHandler:stop()
    hs.timer.doAfter(0.5, function()
                       controlKeyHandler:start()
                       hs.notify.new({title="Hammerspoon", informativeText="EventTap manually restarted"}):send()
    end)
end)

-- Notification on config load
hs.notify.new({title="Hammerspoon", informativeText="Config loaded successfully"}):send()

print("Hammerspoon config loaded")
