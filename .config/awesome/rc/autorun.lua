local autorun_items = {
    "gvim",
    "mkdir -p /tmp/t",
	"dunst -config ~/.dunstrc",
    "xrdb ~/.Xresources",
	"xinput disable $(xinput | egrep -o 'TouchPad.*id=[0-9]*' | egrep -o '[0-9]*')",
    "fcitx-autostart",
	"conky",
    "xrandr --output VGA1 --auto --right-of LVDS1 --rotate normal"
}
local runonce = require("lib/runonce")
for _, item in ipairs(autorun_items) do
    runonce.run(item)
end
