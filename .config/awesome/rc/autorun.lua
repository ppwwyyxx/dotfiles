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

    "sleep 40 && zeal",
    "termite",
    "sleep 100 && redshift-gtk",
    "xscreensaver -nosplash",

    --"/home/wyx/bin/cvim-server.py",
    "/home/wyx/bin/background/notify-daemon.py",

    "sleep 30 && telegram-desktop",
    "sleep 80 && /home/wyx/.local/bin/plaidchat",

    "sleep 100 && dropbox",
}
local runonce = require("lib/runonce")
for _, item in ipairs(autorun_items) do
    runonce.run(item)
end
