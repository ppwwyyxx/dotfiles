
local const = {
default_layout    = awful.layout.suit.floating,
available_layouts = {
    awful.layout.suit.floating,
    awful.layout.suit.spiral,
    awful.layout.suit.magnifier,
},
terminal          = "urxvt",
-- TMP_TERM	      = "TMP_TERMINAL",
browser           = "chromium ",
editor            = "gvim ",
modkey            = "Mod4",
altkey            = "Mod1",
home              = os.getenv("HOME")
}

return const
