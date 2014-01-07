local gears = require("gears")

-- Themes define colours, icons, and wallpapers
beautiful.init(awful.util.getdir("config") .. "/rc/theme.lua")

-- configuration - edit to your liking
local wp_index = 1
local wp_timeout  = 300
local wp_path = awful.util.getdir("config") .. "/wallpaper/"
local wp_files = { "best.png", "best1.png", "best2.png" }

-- use dark.png for second screen
if screen.count() == 2 then
    gears.wallpaper.maximized(wp_path .. "dark.png", 2, true)
end

function changewp()
    local old_index = wp_index
    while wp_index == old_index do
        wp_index = math.random(1, #wp_files)
    end
    gears.wallpaper.maximized(wp_path .. wp_files[wp_index], 1, true)
end

local wp_timer = timer({ timeout = wp_timeout })
wp_timer:connect_signal("timeout", changewp)
wp_timer:start()


-- for fcitx-chttrans
table.insert(naughty.config.icon_dirs, '/usr/share/icons/hicolor/48x48/status/')
