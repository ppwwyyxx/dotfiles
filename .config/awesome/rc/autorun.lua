local autorun_items = {
    "mkdir -p /tmp/t",
	"dunst -config ~/.dunstrc",
    "fcitx-autostart",
    "sogou-qimpanel",
    "conky",
    "xcape -e 'Control_L=Escape;Hyper_L=XF86Mail'",
    "/home/wyx/bin/cvim-server.py",
    "telegram"
}
local runonce = require("lib/runonce")
for _, item in ipairs(autorun_items) do
    runonce.run(item)
end
