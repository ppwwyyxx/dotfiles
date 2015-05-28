local autorun_items = {
    "gvim",
    "mkdir -p /tmp/t",
	"dunst -config ~/.dunstrc",
    "fcitx-autostart",
    "sogou-qimpanel",
    "conky",
    "xcape -e 'Control_L=Escape;Hyper_L=XF86Mail'",
    "megasync",
}
local runonce = require("lib/runonce")
for _, item in ipairs(autorun_items) do
    runonce.run(item)
end
