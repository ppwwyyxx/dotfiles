local myutil = require('lib/myutil')
local gears = require('gears')

-- configuration -
local wp_timeout  = 180
local wp_path = awful.util.getdir("config") .. "/wallpaper/"
local wp_files = myutil.rexec("find -L " .. wp_path .. " -maxdepth 2 -type f | shuf")
wp_files = myutil.split(wp_files, "\n")

-- use dark for second screen
if screen.count() == 2 then
    gears.wallpaper.maximized(wp_path .. "dark.jpg", 2, true)
end

local wp_index = 1
function changewp()
    -- change wallpaper for screen 1
    if #wp_files == 0 then
       myutil.notify("No wallpapers found!", "", "critical")
       return
    end
    wp_index = wp_index % #wp_files + 1
    gears.wallpaper.maximized(wp_files[wp_index], 1, true)
end

local wp_timer = gears.timer({ timeout = wp_timeout })
wp_timer:connect_signal("timeout", changewp)
wp_timer:start()

awful.screen.connect_for_each_screen(function(s)
  if s.index ~= 1 then
    gears.wallpaper.maximized(wp_path .. "dark.jpg", s, true)
  else
    changewp()
  end
end)

-- for fcitx-chttrans ?
table.insert(naughty.config.icon_dirs, '/usr/share/icons/hicolor/48x48/status/')

return {change_wallpaper = changewp }
