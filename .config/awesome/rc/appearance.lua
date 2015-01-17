local gears = require("gears")

-- Themes define colours, icons, and wallpapers
beautiful.init(awful.util.getdir("config") .. "/rc/theme.lua")

-- configuration -
local wp_index = 1
local wp_timeout  = 300
local wp_path = awful.util.getdir("config") .. "/wallpaper/"
--local wp_files = { "best.png", "best1.png", "best2.png", "sky.jpg"}
local wp_files = { "miao.jpg", "sky.jpg" }

-- use dark.png for second screen
if screen.count() == 2 then
    gears.wallpaper.maximized(wp_path .. "dark.png", 2, true)
end

function changewp()
    local old_index = wp_index
    wp_index = (old_index + 1) % #wp_files + 1
--    notify(tostring(wp_index) .. tostring(#wp_files) .. tostring(wp_files[1]))
    gears.wallpaper.maximized(wp_path .. wp_files[wp_index], 1, true)
end

local wp_timer = timer({ timeout = wp_timeout })
wp_timer:connect_signal("timeout", changewp)
wp_timer:start()
changewp()


-- for fcitx-chttrans
table.insert(naughty.config.icon_dirs, '/usr/share/icons/hicolor/48x48/status/')
