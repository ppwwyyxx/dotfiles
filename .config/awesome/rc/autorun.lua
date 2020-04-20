local autorun_items = {
  "mkdir -p /tmp/t && mkdir /t/mnt{,2,3} /t/mac",
  "xcape -e 'Control_L=Escape;Hyper_L=XF86Mail'",

  -- hardware:
  --"wicd-gtk",
  "blueman-applet",
  "nvidia-smi",

  -- UI:
  "sleep 3 && compton --config ~/.compton.conf",  -- https://github.com/awesomeWM/awesome/issues/1660
  "conky",
  -- "sleep 100 && conky -c /home/wyx/.conkyrc-cal",
  "dunst -config ~/.dunstrc",
  -- "xscreensaver -nosplash",
  "gnome-screensaver",

  -- IM:
  "fcitx-autostart",
  "sogou-qimpanel",

  -- Software:
  "sleep 40 && zeal",
  "termite",
  --"/home/wyx/bin/cvim-server.py",
  "sleep 180 && /home/wyx/bin/background/notify-daemon.py",
  "sleep 30 && QT_STYLE_OVERRIDE= QT_SCALE_FACTOR= telegram-desktop",
  --"/usr/lib/gsd-xsettings",  -- needed for some wine apps
  "sleep 30 && /home/wyx/.dropbox-dist/dropboxd",

  -- https://github.com/jonls/redshift/issues/636
  "/usr/lib/geoclue-2.0/demos/agent",
  "sleep 10 && redshift-gtk",
  "/opt/Todoist/todoist",

}

local runonce = require("lib/runonce")
do
   for _, item in ipairs(autorun_items) do
       runonce.run(item)
   end
end
