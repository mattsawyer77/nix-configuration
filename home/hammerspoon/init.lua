-- Hammerspoon Configuration
-- Left CTRL acts as ESC when tapped, CTRL when held
-- Firefox-specific: CTRL+space -> CMD+l, %, space

-- Track left CTRL key state
local controlPressed = false
local controlUsedWithOtherKey = false

-- Left CTRL key event handler
controlKeyHandler = hs.eventtap.new({hs.eventtap.event.types.flagsChanged, hs.eventtap.event.types.keyDown}, function(event)
    local flags = event:getFlags()
    local keyCode = event:getKeyCode()
    
    -- Check if left control key (keyCode 59)
    if keyCode == 59 then
        if event:getType() == hs.eventtap.event.types.flagsChanged then
            if flags.ctrl then
                -- Left CTRL pressed
                controlPressed = true
                controlUsedWithOtherKey = false
            else
                -- Left CTRL released
                if controlPressed and not controlUsedWithOtherKey then
                    -- Tap ESC
                    hs.eventtap.keyStroke({}, "escape")
                end
                controlPressed = false
                controlUsedWithOtherKey = false
            end
        end
        -- Don't suppress the original control event
        return false
    end
    
    -- If any other key is pressed while control is held
    if controlPressed and event:getType() == hs.eventtap.event.types.keyDown then
        controlUsedWithOtherKey = true
    end
    
    return false
end)

controlKeyHandler:start()

-- Firefox-specific: CTRL+space mapping
firefoxHotkey = hs.hotkey.bind({"ctrl"}, "space", function()
    local app = hs.application.frontmostApplication()
    if app:name() == "Firefox" then
        -- Send CMD+l (focus address bar)
        hs.eventtap.keyStroke({"cmd"}, "l")
        hs.timer.usleep(50000) -- 50ms delay
        
        -- Send %
        hs.eventtap.keyStrokes("%")
        hs.timer.usleep(50000) -- 50ms delay
        
        -- Send space
        hs.eventtap.keyStroke({}, "space")
    else
        -- If not Firefox, send regular CTRL+space
        hs.eventtap.keyStroke({"ctrl"}, "space")
    end
end)

-- Notification on config load
hs.notify.new({title="Hammerspoon", informativeText="Config loaded successfully"}):send()

print("Hammerspoon config loaded:")
print("- Left CTRL: tap for ESC, hold for CTRL")
print("- Firefox CTRL+space: CMD+l, %, space")
