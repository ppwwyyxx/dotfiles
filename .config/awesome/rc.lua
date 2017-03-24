-- Standard awesome library
awful = require("awful")
awful.rules = require("awful.rules")
gears = require('gears')
gears.table = awful.util.table  -- TODO wait for update
require("awful.autofocus")
beautiful = require("beautiful")
naughty = require("naughty")
-- Themes define colours, icons, and wallpapers
beautiful.init(awful.util.getdir("config") .. "/rc/theme.lua")

os.setlocale("")
dbus.release_name("session", "org.freedesktop.Notifications")

ROOT_KEYS = {}

require("rc/error")
require("rc/autorun")
appearance = require("rc/appearance")
menu = require("rc/menu")
tags = require("rc/tags")  -- screen -> table of tags
bar = require("rc/bar")
keys = require("rc/keys")
rules = require("rc/rules")

root.keys(ROOT_KEYS)
