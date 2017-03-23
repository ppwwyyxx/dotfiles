-- Standard awesome library
awful = require("awful")
awful.rules = require("awful.rules")
require("awful.autofocus")
naughty = require("naughty")

beautiful = require("beautiful")
-- Themes define colours, icons, and wallpapers
beautiful.init(awful.util.getdir("config") .. "/rc/theme.lua")

os.setlocale("")
-- dbus.release_name("session", "org.freedesktop.Notifications")

-- Simple function to load additional LUA files from rc/.
function loadrc(name, module)
   local path = awful.util.getdir("config") .. "/rc/" ..  name .. ".lua"
   local success
   local result
   success, result = pcall(function() return dofile(path) end)
   if not success then
      naughty.notify({ title = "Error while loading an RC file",
		       text = "When loading `" .. name ..
			  "`, got the following error:\n" .. result,
		       preset = naughty.config.presets.critical
		     })
      return print("E: error loading RC file '" .. name .. "': " .. result)
   end
   return result
end

config = {}
config.layouts = {
    awful.layout.suit.floating,
    awful.layout.suit.spiral,
    awful.layout.suit.magnifier,
}
config.global = {}

require("rc/error")
require("rc/autorun")
appearance = require("rc/appearance")
loadrc("tags")
loadrc("menu")
loadrc("bar")
loadrc("keys")
loadrc("rules")

root.keys(config.global)
