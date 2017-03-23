-- xdg_menu --format awesome > ~/.config/awesome/lib/xdgmenu.lua

xdgmenu = require('lib/xdgmenu')
local const = require('rc/const')

local awesome_menu = {
   { "Edit Config (&E)", const.editor .. awesome.conffile },
   { "Restart (&R)", awesome.restart, '/usr/share/icons/gnome/16x16/actions/stock_refresh.png' },
   { "Logout (&L)", awesome.quit },
}

local main_menu = awful.menu({ items = {
          { "Awesome", awesome_menu, beautiful.awesome_icon },
          { "Terminal", const.terminal, '/usr/share/icons/gnome/32x32/apps/utilities-terminal.png' },
          { "&Chromium", "chromium", '/usr/share/icons/hicolor/32x32/apps/chromium.png' },
          { "&Wallpaper", appearance.change_wallpaper},
          { "XDG Menu (&A)", xdgmenu},
          { "Suspend (&S)", "systemctl suspend" },
      }
})

-- Set the terminal for applications that require it
local menubar = require("menubar")
menubar.utils.terminal = terminal

return {main_menu = main_menu}
