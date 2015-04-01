local autorun_items = {
    "gvim",
    "mkdir -p /tmp/t",
	"dunst -config ~/.dunstrc",
    "xrdb ~/.Xresources",
    "fcitx-autostart",
    "sogou-qimpanel",
    "conky",
    "xrandr --output VGA1 --auto --right-of LVDS1 --rotate normal",
    "xcape -e 'Control_L=Escape'",
    "xset r rate 200 40",
}
local runonce = require("lib/runonce")
for _, item in ipairs(autorun_items) do
    runonce.run(item)
end
