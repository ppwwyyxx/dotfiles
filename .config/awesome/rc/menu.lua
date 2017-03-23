-- xdg_menu --format awesome > ~/.config/awesome/lib/xdgmenu.lua

require('lib/xdgmenu')
local menubar = require("menubar")
local const = require('rc/const')

local my_awesomemenu = {
   { "Edit Config (&E)", const.editor .. awesome.conffile },
   { "Restart (&R)", awesome.restart, '/usr/share/icons/gnome/16x16/actions/stock_refresh.png' },
   { "Logout (&L)", awesome.quit },
}

my_mainmenu = awful.menu({ items = {
          { "Awesome", my_awesomemenu, beautiful.awesome_icon },
          { "Terminal", const.terminal, '/usr/share/icons/gnome/32x32/apps/utilities-terminal.png' },
          { "G&VIM", "gvim", '/usr/share/pixmaps/gvim.png' },
          { "&Chromium", "chromium", '/usr/share/icons/hicolor/32x32/apps/chromium.png' },
          { "&Wallpaper", appearance.change_wallpaper},
          { "Applications (&A)", xdgmenu},
          { "Suspend (&S)", "systemctl suspend" },
      }
})

-- Set the terminal for applications that require it
menubar.utils.terminal = terminal
