local autorun_items = {
    "compton --config ~/.compton.conf",
    "mkdir -p /tmp/t",
	"dunst -config ~/.dunstrc",
    "fcitx-autostart",
    "sogou-qimpanel",
    "conky",
    "conky -c /home/wyx/.conkyrc-cal",
    "xcape -e 'Control_L=Escape;Hyper_L=XF86Mail'",
    "wicd-client -t",

    "zeal",
    "urxvt",

    "/home/wyx/bin/cvim-server.py",
    "/home/wyx/bin/notify-daemon.py",

    "telegram",
    "plaidchat",

    "dropbox",
}
local runonce = require("lib/runonce")
for _, item in ipairs(autorun_items) do
    runonce.run(item)
end
