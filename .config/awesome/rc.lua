-- Standard awesome library
require("awful")
awful = require("awful")
awful.rules = require("awful.rules")
require("awful.autofocus")
beautiful = require("beautiful")
naughty = require("naughty")

os.setlocale("")
dbus.release_name("session", "org.freedesktop.Notifications")


-- Simple function to load additional LUA files from rc/.
function loadrc(name, module)
   local path = awful.util.getdir("config") .. "/" ..
      (module and "lib" or "rc") ..  "/" .. name .. ".lua"

   -- Don't load it again
   if module and package.loaded[module] then return package.loaded[module] end

   local success
   local result
   -- Execute the RC/module file
   success, result = pcall(function() return dofile(path) end)
   if not success then
      naughty.notify({ title = "Error while loading an RC file",
		       text = "When loading `" .. name ..
			  "`, got the following error:\n" .. result,
		       preset = naughty.config.presets.critical
		     })
      return print("E: error loading RC file '" .. name .. "': " .. result)
   end

   if module then
      return package.loaded[module]
   end
   return result
end

config = {}
config.layouts = {
-- for fcitx-chttrans
    awful.layout.suit.floating,
    awful.layout.suit.tile,
    awful.layout.suit.tile.top,
    awful.layout.suit.max,
}
config.global = {}
config.global = {}

terminal          = "urxvt"
TMP_TERM	      = "TMP_TERMINAL"
browser           = "chromium "
editor            = "gvim "
modkey            = "Mod4"
altkey            = "Mod1"
last_tag          = 6
home              = os.getenv("HOME")

loadrc("common")
loadrc("error")

loadrc("autorun")
loadrc("appearance")
loadrc("tags")
loadrc("menu")
loadrc("bar")
loadrc("keys")
loadrc("rules")

root.keys(config.global)
