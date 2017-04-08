local autorun_items = {
  "mkdir -p /tmp/t && mkdir /t/mnt{,2,3}",
  "xcape -e 'Control_L=Escape;Hyper_L=XF86Mail'",
  -- "wicd-client -t",

  "sleep 3 && compton --config ~/.compton.conf",  -- https://github.com/awesomeWM/awesome/issues/1660
	"dunst -config ~/.dunstrc",
  "xscreensaver -nosplash",
  "fcitx-autostart",
  "sogou-qimpanel",
  "conky",
  -- "sleep 100 && conky -c /home/wyx/.conkyrc-cal",


  "sleep 40 && zeal",
  "termite",
  "nvidia-smi",

  --"/home/wyx/bin/cvim-server.py",
  "/home/wyx/bin/background/notify-daemon.py",
  "sleep 30 && telegram-desktop",

  --"sleep 100 && dropbox",
  --"sleep 100 && nocturn",

  "sleep 10 && redshift-gtk",
  "sleep 15 && workrave"
}

local runonce = require("lib/runonce")
do
   for _, item in ipairs(autorun_items) do
       runonce.run(item)
   end
end
