-- xdg_menu --format awesome > ~/.config/awesome/lib/xdgmenu.lua

require('lib/xdgmenu')
menubar = require("menubar")

local my_awesomemenu = {
   { "编辑配置 (&E)", editor .. awesome.conffile },
   { "重新加载 (&R)", awesome.restart, '/usr/share/icons/gnome/16x16/actions/stock_refresh.png' },
   { "注销 (&L)", awesome.quit },
}

my_mainmenu = awful.menu({ items = {
          { "Awesome", my_awesomemenu, beautiful.awesome_icon },
          { "终端 (&T)", terminal, '/usr/share/icons/gnome/32x32/apps/utilities-terminal.png' },
          { "G&VIM", "gvim", '/usr/share/pixmaps/gvim.png' },
          { "&Chromium", "chromium", '/usr/share/icons/hicolor/32x32/apps/chromium.png' },
          { "&Wallpaper", changewp},
          { "应用程序 (&A)", xdgmenu},
          { "挂起 (&S)", "systemctl suspend" },
      }
})

menubar.utils.terminal = terminal -- Set the terminal for applications that require it
