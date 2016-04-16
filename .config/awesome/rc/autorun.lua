local autorun_items = {
    "compton --config ~/.compton.conf",
    "mkdir -p /tmp/t",
	 "dunst -config ~/.dunstrc",
    "fcitx-autostart",
    "sogou-qimpanel",
    "conky",
    "sleep 100 && conky -c /home/wyx/.conkyrc-cal",
    "xcape -e 'Control_L=Escape;Hyper_L=XF86Mail'",
    "wicd-client -t",

    "sleep 50 && zeal",
    "termite",

    "/home/wyx/bin/cvim-server.py",
    "/home/wyx/bin/notify-daemon.py",

    "sleep 50 && telegram-desktop",
    "sleep 50 && /home/wyx/.local/bin/plaidchat",

    --"sleep 100 && dropbox",
}
local runonce = require("lib/runonce")
for _, item in ipairs(autorun_items) do
    runonce.run(item)
end
