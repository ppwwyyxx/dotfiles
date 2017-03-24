-- xdg_menu --format awesome > ~/.config/awesome/lib/xdgmenu.lua

local gears = require('gears')
local xdgmenu = require('lib/xdgmenu')
local const = require('rc/const')

local awesome_menu = {
   { "Edit Config (&E)", const.editor .. awesome.conffile },
   { "Restart (&R)", awesome.restart, '/usr/share/icons/gnome/16x16/actions/stock_refresh.png' },
   { "Logout (&L)", awesome.quit },
}

local main_menu = awful.menu({ items = {
          { "&Awesome", awesome_menu, beautiful.awesome_icon },
          { "&Terminal", const.terminal, '/usr/share/icons/gnome/32x32/apps/utilities-terminal.png' },
          { "&Chromium", "chromium", '/usr/share/icons/hicolor/32x32/apps/chromium.png' },
          { "&Wallpaper", appearance.change_wallpaper},
          { "&XDG Menu", xdgmenu},
          { "&Suspend", "systemctl suspend" },
      }
})

-- Set the terminal for applications that require it
local menubar = require("menubar")
menubar.utils.terminal = terminal

root.buttons(gears.table.join(
	awful.button({}, 1, function() main_menu:hide() end),
	awful.button({}, 3, function() main_menu:toggle() end),
	awful.button({}, 4, awful.tag.viewprev),
	awful.button({}, 5, awful.tag.viewnext)
))

return {main_menu = main_menu}
