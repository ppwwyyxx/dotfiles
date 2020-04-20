local myutil = require('lib/myutil')
local gears = require('gears')

-- configuration -
local wp_timeout  = 180
local wp_path = awful.util.getdir("config") .. "/wallpaper/"
local wp_files = myutil.rexec("find -L " .. wp_path .. " -maxdepth 3 -type f \\( -name '*.jpg' -or -name '*.png' \\) | shuf")
wp_files = myutil.split(wp_files, "\n")
if #wp_files == 0 then
  myutil.notify("No wallpapers found!", "", "critical")
end


local wp_index = 1
function changewp()
    -- change wallpaper for screen 1
    local target = myutil.find_largest_screen()
    for s in screen do
      if s.index ~= target.index then
      -- use dark for other screen
        gears.wallpaper.maximized(wp_path .. "dark.jpg", s.index, true)
      else
        if #wp_files == 0 then
          return
        end
        wp_index = wp_index % #wp_files + 1
        gears.wallpaper.maximized(wp_files[wp_index], s.index, true)
      end
    end
end

local wp_timer = gears.timer({ timeout = wp_timeout })
wp_timer:connect_signal("timeout", changewp)
wp_timer:start()


awful.screen.connect_for_each_screen(function(s)
  changewp()
end)

-- for fcitx-chttrans ?
table.insert(naughty.config.icon_dirs, '/usr/share/icons/hicolor/48x48/status/')

return {change_wallpaper = changewp }
