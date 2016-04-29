local gears = require("gears")

-- Themes define colours, icons, and wallpapers
beautiful.init(awful.util.getdir("config") .. "/rc/theme.lua")

local function split(str, sep)
   local result = {}
   local regex = ("([^%s]+)"):format(sep)
   for each in str:gmatch(regex) do
      table.insert(result, each)
   end
   return result
end

-- configuration -
local wp_index = 1
local wp_timeout  = 180
local wp_path = awful.util.getdir("config") .. "/wallpaper/"
local wp_files = rexec("find -L " .. wp_path .. " -maxdepth 2 -type f | shuf")
wp_files = split(wp_files, "\n")

-- use dark.png for second screen
if screen.count() == 2 then
    gears.wallpaper.maximized(wp_path .. "dark.jpg", 2, true)
end

function changewp()
    wp_index = wp_index % #wp_files + 1
    gears.wallpaper.maximized(wp_files[wp_index], 1, true)
end

local wp_timer = timer({ timeout = wp_timeout })
wp_timer:connect_signal("timeout", changewp)
wp_timer:start()
changewp()


-- for fcitx-chttrans
table.insert(naughty.config.icon_dirs, '/usr/share/icons/hicolor/48x48/status/')
