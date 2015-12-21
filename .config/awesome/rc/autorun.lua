local autorun_items = {
    "compton --config ~/.compton.conf",
    "mkdir -p /tmp/t",
	"dunst -config ~/.dunstrc",
    "fcitx-autostart",
    "sogou-qimpanel",
    "conky",
    "conky -c /home/wyx/.conkyrc-cal",
    "xcape -e 'Control_L=Escape;Hyper_L=XF86Mail'",
    "telegram",
    "/home/wyx/bin/cvim-server.py",
    "dropbox",
    "wicd-client -t",
    "zeal",
    "urxvt",
    "mutate"
}
local runonce = require("lib/runonce")
for _, item in ipairs(autorun_items) do
    runonce.run(item)
end
