-- Standard awesome library
gears = require("gears")
awful = require("awful")
awful.rules = require("awful.rules")
require("awful.autofocus")
-- Widget and layout library
wibox = require("wibox")
vicious = require("vicious")
beautiful = require("beautiful")
naughty = require("naughty")
menubar = require("menubar")
-- for fcitx-chttrans
table.insert(naughty.config.icon_dirs, '/usr/share/icons/hicolor/48x48/status/')

local myutil = require("myutil")
local fixwidthtextbox = require("fixwidthtextbox")
local menu = require("menu")

os.setlocale("")
-- A debugging func
n = function(n) naughty.notify{title="消息", text=tostring(n)} end

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function(err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = err })
        in_error = false
    end)
end
-- }}}

-- Themes define colours, icons, and wallpapers
beautiful.init(awful.util.getdir("config") .. "/theme.lua")

local terminal = "urxvt"
local tmp_terminal_name = "TMP_TERMINAL"
local browser = "chromium"
local editor = "gvim"
local modkey = "Mod4"
local altkey = "Mod1"
local last_tag = 6
local home   = os.getenv("HOME")
local exec   = awful.util.spawn
local sexec  = exec_with_shell
local scount = screen.count()

-- Table of layouts to cover with awful.layout.inc, order matters.
local layouts = {
    awful.layout.suit.floating,
    awful.layout.suit.tile,
    awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.tile.top,
    awful.layout.suit.max,
    awful.layout.suit.max.fullscreen,
    awful.layout.suit.magnifier,
}

if beautiful.wallpaper then
    for s = 1, screen.count() do
        gears.wallpaper.maximized(beautiful.wallpaper, s, true)
    end
end

-- Tags
-- Define a tag table which hold all screen tags.
tags_name = { "1", "2", "3", "4", "5", "0"}
tags_layout = {
    awful.layout.suit.floating,
    awful.layout.suit.floating,
    awful.layout.suit.floating,
    awful.layout.suit.floating,
    awful.layout.suit.floating,
    awful.layout.suit.floating,
}
tags = {}
revtags = {}
for s = 1, screen.count() do
    -- Each screen has its own tag table.
    tags[s] = awful.tag(tags_name, s, tags_layout)
    revtags[s] = {}
    for i, t in ipairs(tags[s]) do
        revtags[s][t] = i
    end
end

-- Menu f[[
local my_awesomemenu = {
   { "编辑配置 (&E)", editor .. " " .. awesome.conffile },
   { "重新加载 (&R)", awesome.restart, '/usr/share/icons/gnome/16x16/actions/stock_refresh.png' },
   { "注销 (&L)", awesome.quit },
}

my_mainmenu = awful.menu({ items = { { "Awesome", my_awesomemenu, beautiful.awesome_icon },
          { "终端 (&T)", terminal, '/usr/share/icons/gnome/32x32/apps/utilities-terminal.png' },
          { "G&VIM", "gvim", '/usr/share/pixmaps/gvim.png' },
          { "&Chromium", "chromium", '/usr/share/icons/hicolor/32x32/apps/chromium.png' },
          { "应用程序 (&A)", xdgmenu},
          { "挂起 (&S)", "systemctl suspend" },
          { "关机 (&H)", "zenity --question --title '关机' --text '你确定关机吗？' --default-no && systemctl poweroff", '/usr/share/icons/gnome/16x16/actions/gtk-quit.png' },
      }
})

my_launcher = awful.widget.launcher({ image = beautiful.awesome_icon, menu = my_mainmenu })

menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- f]]

-- Bar f[[
my_textclock = awful.widget.textclock(" %m-%d %H:%M %A ", 1)

cpugraph  = awful.widget.graph()
cpugraph:set_width(40):set_height(14)
cpugraph:set_background_color(beautiful.fg_off_widget)
vicious.register(cpugraph,  vicious.widgets.cpu, "$1")

temp_widget = wibox.widget.textbox()
vicious.register(temp_widget, vicious.widgets.thermal, " $1℃", 19, "thermal_zone0")

mem_widget = wibox.widget.textbox()
vicious.register(mem_widget, vicious.widgets.mem, '<span color="#90ee90">Mem $1%</span>')

-- Network f[[
net_widget = wibox.widget.textbox()
function update_netstat()
    local netif
    local f = io.open('/proc/net/route')
    for line in f:lines() do
        netif = line:match('^(%w+)%s+00000000%s')
        if netif then break end
    end
    f:close()

	if not netif then
		vicious.unregister(net_widget, false)
		vicious.register(net_widget, vicious.widgets.net, 'No Network', 3)
	else
		vicious.unregister(net_widget, false)
		vicious.register(net_widget, vicious.widgets.net,
				   '<span color="#5798d9">↓${' .. netif .. ' down_kb}</span> <span color="#c2ba62">↑${'.. netif ..' up_kb}</span>', 1)
	end
end
net_widget_clock = timer({ timeout = 100 })
net_widget_clock:connect_signal("timeout", update_netstat)
net_widget_clock:start()
update_netstat()
-- f]]

--Battery f[[
local battery_state = {
	unknown     = '<span color="yellow">? ',
	idle        = '<span color="#0000ff">↯',
	charging    = '<span color="green">+ ',
	discharging = '<span color="#1e90ff">– ',
}

function update_bat_widget()
	local bat_dir = '/sys/devices/platform/smapi/BAT0/'
	local f = io.open(bat_dir .. 'state')
	if not f then
		bat_widget:set_markup('<span color="red">No Battery</span>')
		return
	end
	local state = f:read()
	f:close()
	local state_text = battery_state[state] or battery_state.unknown

	f = io.open(bat_dir .. 'remaining_percent')
	local percent = tonumber(f:read())
	f:close()
	if percent <= 25 then
		if state == 'discharging' then
			local t = os.time()
			if t - last_bat_warning > 300 then
				naughty.notify{
					preset = naughty.config.presets.critical,
					title = "Low Battery", text = 'Battery: ' .. percent .. '% ！',
				}
				last_bat_warning = t
			end
		end
		percent = '<span color="red">' .. percent .. '</span>'
	end
	percent = (percent == 100) and "" or percent .. '%'
	bat_widget:set_markup(state_text .. percent .. '</span>')
end
local last_bat_warning = 0
bat_widget = fixwidthtextbox('↯??%')
bat_widget.width = 55
bat_widget:set_align('center')
bat_clock = timer({ timeout = 20 })
bat_clock:connect_signal("timeout", update_bat_widget)
bat_clock:start()
-- f]]

--Volume f[[
function volumectl (mode, widget)
	if mode == "update" then
		local f = io.popen("pamixer --get-volume")
		local volume = f:read()
		f:close()
		if not tonumber(volume) then
			widget:set_markup("<span color='red'>ERR</span>")
			return
		end
		volume = string.format("% 3d", volume)

		f = io.popen("pamixer --get-mute")
		local muted = f:read('*all')
		f:close()
		volume = '♫' .. volume .. ((muted == "false") and "%" or "<span color='red'>M</span>")
		widget:set_markup(volume)
	elseif mode == "up" then
		local f = io.popen("pamixer --allow-boost --increase 5")
		f:close()
		volumectl("update", widget)
	elseif mode == "down" then
		local f = io.popen("pamixer --allow-boost --decrease 5")
		f:close()
		volumectl("update", widget)
	else
		local f = io.popen("pamixer --toggle-mute")
		f:close()
		volumectl("update", widget)
	end
end
volume_clock = timer({ timeout = 10 })
volume_clock:connect_signal("timeout", function() volumectl("update", volume_widget) end)
volume_clock:start()

volume_widget = fixwidthtextbox('(volume)')
volume_widget.width = 68
volume_widget:set_align('center')
volume_widget:buttons(awful.util.table.join(
	awful.button({ }, 4, function() volumectl("up", volume_widget) end),
	awful.button({ }, 5, function() volumectl("down", volume_widget) end),
	awful.button({ }, 3, function() exec("pavucontrol") end),
	awful.button({ }, 1, function() volumectl("mute", volume_widget) end)
	))
-- f]]

-- Taglist, Tasklist f[[
my_taglist = {}
my_taglist.buttons = awful.util.table.join(
	awful.button({ }, 1, awful.tag.viewonly),
	awful.button({ modkey }, 1, awful.client.movetotag),
	awful.button({ }, 3, awful.tag.viewtoggle),
	awful.button({ modkey }, 3, awful.client.toggletag),
	awful.button({ }, 4, function(t) awful.tag.viewnext(awful.tag.getscreen(t)) end),
	awful.button({ }, 5, function(t) awful.tag.viewprev(awful.tag.getscreen(t)) end)
	)
my_tasklist = {}
my_tasklist.buttons = awful.util.table.join(
	awful.button({ }, 1,
			  function(c)
				  if c == client.focus and not c.minimized then
					  c.minimized = true
				  else
					  -- Without this, the following :isvisible() makes no sense
					  c.minimized = false
					  if not c:isvisible() then
						  awful.tag.viewonly(c:tags()[1])
					  end
					  -- This will also un-minimize the client, if needed
					  client.focus = c
					  c:raise()
				  end
			  end),
	awful.button({ }, 2, function(c) c:kill() end),
	awful.button({ }, 3, function()
			  if instance then
				  instance:hide()
				  instance = nil
			  else
				  instance = awful.menu.clients({ width=250 })
			  end
		  end),
	awful.button({ }, 4, function()
			  awful.client.focus.byidx(1)
			  if client.focus then client.focus:raise() end
		  end),
	awful.button({ }, 5, function()
			  awful.client.focus.byidx(-1)
			  if client.focus then client.focus:raise() end
		  end)
)
-- f]]

-- WiBox f[[
my_wibox = {}
my_promptbox = {}
my_layoutbox = {}
for s = 1, screen.count() do
	my_layoutbox[s] = awful.widget.layoutbox(s)
	my_layoutbox[s]:buttons(awful.util.table.join(
		awful.button({ }, 1, function() awful.layout.inc(layouts, 1) end),
		awful.button({ }, 3, function() awful.layout.inc(layouts, -1) end),
		awful.button({ }, 4, function() awful.layout.inc(layouts, 1) end),
		awful.button({ }, 5, function() awful.layout.inc(layouts, -1) end)
		))
	my_taglist[s] = awful.widget.taglist(s, awful.widget.taglist.filter.all, my_taglist.buttons)
	my_tasklist[s] = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, my_tasklist.buttons)
	my_wibox[s] = awful.wibox({ position = "top", screen = s, height = 20 })
	my_promptbox[s] = awful.widget.prompt()

	local left_layout = wibox.layout.fixed.horizontal()
	left_layout:add(my_launcher)
	left_layout:add(my_taglist[s])
	left_layout:add(my_promptbox[s])

	local right_layout = wibox.layout.fixed.horizontal()
	right_layout:add(cpugraph)
	right_layout:add(temp_widget)
	right_layout:add(mem_widget)
	right_layout:add(bat_widget)
	right_layout:add(net_widget)
	right_layout:add(volume_widget)
	if s == 1 then
		right_layout:add(wibox.widget.systray())
	end
	right_layout:add(my_textclock)
	right_layout:add(my_layoutbox[s])

	local layout = wibox.layout.align.horizontal()
	layout:set_left(left_layout)
	layout:set_middle(my_tasklist[s])
	layout:set_right(right_layout)

	my_wibox[s]:set_widget(layout)
end
-- f]]
-- f]]

-- Root Keys/Buttons: f[[
-- Global Keys f[[
function moveresize_abs(x, y, w, c)
	local g = c:geometry()
    local scr = screen[c.screen].workarea
	if x < 0 then x = scr.x + scr.width + x end
	if w == 0 then w = g.width end
	awful.client.moveresize(-g.x + scr.x + x, -g.y + scr.y + y,
						-g.width + w, -g.height + scr.height, c)
end

globalkeys = awful.util.table.join(
	awful.key({ modkey,           }, "w", function() my_mainmenu:show() end),
	awful.key({ modkey,           }, "n", function() awful.screen.focus_relative(1) end),
	-- awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),

	-- Layout manipulation for tiling
	awful.key({ modkey,           }, "l",     function() awful.tag.incmwfact( 0.05)    end),
	awful.key({ modkey,           }, "h",     function() awful.tag.incmwfact(-0.05)    end),

	awful.key({modkey, }, ";", function()
		   local c = client.focus
		   moveresize_abs(0, 0, 0, c)
	end),
	awful.key({modkey, }, "'", function()
		   local c = client.focus
		   moveresize_abs(-c:geometry().width, 0, 0, c)
	   end),

	awful.key({modkey, "Shift"}, "'", function()
		  keygrabber.run(function(mod, key, event)
				   if event == "release" then return end
				   if     key == 'k' then awful.client.moveresize(0, -7, 0, 0)
				   elseif key == 'j' then awful.client.moveresize(0, 7, 0, 0)
				   elseif key == 'l' then awful.client.moveresize(7, 0, 0, 0)
				   elseif key == 'h' then awful.client.moveresize(-7, 0, 0, 0)
				   elseif key == ',' then awful.client.moveresize(-20, 0, 0, 0)
				   elseif key == '.' then awful.client.moveresize(20, 0, 0, 0)
				   else keygrabber.stop() end
			   end)
	 end),


	awful.key({ modkey,           }, "space", function() awful.layout.inc(layouts,  1) end),
	awful.key({ modkey, "Shift"   }, "space", function() awful.layout.inc(layouts, -1) end),

	-- toggle sticky for unfocusable object under mouse
	awful.key({ modkey, "Shift"   }, "s",
		function()
			local c = mouse.object_under_pointer()
			if c then c.sticky = not c.sticky end
		end),

	-- terrible things
	awful.key({ altkey, "Control", "Shift"}, "r", awesome.restart),
	awful.key({ altkey, "Control", "Shift"}, "q", awesome.quit),
	awful.key({ altkey, "Control", "Shift" }, "k", function() exec("xkill") end),
	awful.key({altkey, "Control", "Shift"}, "Print", function()
		exec("zsh -c 'cd /tmp\nscrot\n'")
		os.execute("sleep .5")
		naughty.notify({title="Screenshot", text="Screenshot saved to /tmp"})
	end),

	-- TODO
	-- awful.key({ modkey, "Shift"   }, "x", function() exec('openmsg_tm.py', false) end),

	-- Switching tags
	awful.key({ modkey }, "Left",   awful.tag.viewprev       ),
	awful.key({ modkey }, "Right",  awful.tag.viewnext       ),
	awful.key({ modkey }, "Escape", awful.tag.history.restore),


	-- Alt-Tab
	awful.key({ altkey,          }, "Tab", function()
		awful.client.focus.byidx(1)
		if client.focus then client.focus:raise() end
	end),
	awful.key({ altkey, "Shift"  }, "Tab", function()
		awful.client.focus.byidx(-1)
		if client.focus then client.focus:raise() end
	end),
	awful.key({ modkey,           }, "Tab", function()
		awful.client.focus.history.previous()
		if client.focus then client.focus:raise() end
	end),

	awful.key({ modkey, }, "d", function()
		local curtag
		local curtags = awful.tag.selectedlist()
		local client
		local clients
		local allminimized

		for x, curtag in pairs(curtags) do
			clients = curtag:clients()
			for y, client in pairs(clients) do
				if client.minimized == false then
					allminimized = false
					break
				else
					allminimized = true
				end
			end

			-- If at least one client isn't minimized, minimize all clients
			for y, client in pairs(clients) do
				if allminimized == false then
					client.minimized = true

					-- Otherwise unminimize all clients
				elseif allminimized == true then
					client.minimized = false
				end
			end
		end
	end),

	-- Common program
	awful.key({ modkey,   }, "Return", function() myutil.run_or_raise("urxvt -name '" .. tmp_terminal_name .. "'", {name = tmp_terminal_name}) end),
	awful.key({ modkey    }, "r",     function() my_promptbox[mouse.screen]:run() end),     -- TODO: change launcher
	awful.key({ modkey,   }, "g", function() exec("sudo gnome-control-center") end),
	-- awful.key({ "Control", altkey }, "l", function() exec("leave") end),
	-- awful.key({ modkey,           }, "x", function() exec("openmsg.py", false) end),
	awful.key({ modkey,   }, "t", function() exec(terminal) end),
	awful.key({ modkey,   }, "c", function() exec("chromium") end),
	awful.key({ modkey,   }, "f", function() exec("firefox") end),
	awful.key({ modkey,   }, "a", function() exec(home .. "/bin/background/screenshot") end),

	-- htop
	awful.key({ modkey,   }, "z", function()
		if client.focus and client.focus.name == 'htop' then
			awful.client.movetotag(tags[mouse.screen][last_tag], client.focus)
		else
			myutil.run_or_raise("urxvt -e 'htop'", { name = "htop" })
		end
	end),

	awful.key({ modkey,   }, "q", function()
		local c = client.focus
		if not c then return end
		if c.name == 'htop' or c.name == tmp_terminal_name then
			awful.client.movetotag(tags[mouse.screen][last_tag], c)
		else
			c:kill()
		end
	end),

	-- sdcv
	awful.key({ altkey, }, "F3", function()
		local new_word = selection()
		if _dict_notify ~= nil then
			naughty.destroy(_dict_notify)
			_dict_notify = nil
			if _old_word == new_word then return end
		end
		_old_word = new_word

		local f  = io.popen("sdcv -n --utf8-output '"..new_word.."'")
		local ans = f:read('*all')
		f:close()
		_dict_notify = naughty.notify({ text = ans, timeout = 5, width = 1020 })
	end),
	awful.key({ altkey, "Shift"}, "F3", function()
		awful.prompt.run({ prompt = "Dictionary: " }, my_promptbox[mouse.screen].widget,
				   function (words)
					   _old_word = words
					   naughty.notify({ text = words, timeout = 5, width = 1020 })
					   local f  = io.popen("sdcv -n --utf8-output '" .. words .. "'")
					   local ans = f:read('*all')
					   f:close()
					   _dict_notify = naughty.notify({ text = ans, timeout = 5, width = 1020 })
				   end)
	end),

	-- Volume
	awful.key({ }, 'XF86AudioRaiseVolume', function() volumectl("up", volume_widget) end),
	awful.key({ }, 'XF86AudioLowerVolume', function() volumectl("down", volume_widget) end),
	awful.key({ }, 'XF86AudioMute', function() volumectl("mute", volume_widget) end)
) -- f]]

-- Tag Number Keys  f[[
local register_tagkey = function(key, index)
	globalkeys = awful.util.table.join(
		globalkeys,
		awful.key({ modkey }, key, function()      -- view only
	  local screen = mouse.screen
	  if tags[screen][index] then
		  awful.tag.viewonly(tags[screen][index])
	  end
  end),
	awful.key({ modkey, "Control" }, key, function()    -- toggle view
	 local screen = mouse.screen
	 if tags[screen][index] then
		 awful.tag.viewtoggle(tags[screen][index])
	 end
 end),
	awful.key({ modkey, "Shift"   }, key, function()        -- move but not jump
	 if client.focus and tags[client.focus.screen][index] then
		 awful.client.movetotag(tags[client.focus.screen][index])
	 end
 end),
	awful.key({ modkey, "Control", "Shift" }, key, function()
	 if client.focus and tags[client.focus.screen][index] then
		 awful.client.toggletag(tags[client.focus.screen][index])
	 end
 end)
	)
end
do
	local keynumber = last_tag - 1
	for key = 1, keynumber do
		register_tagkey(key, key)
	end
	register_tagkey(0, last_tag)
end
-- f]]

root.keys(globalkeys)
root.buttons(awful.util.table.join(
	awful.button({ }, 3, function() my_mainmenu:toggle() end),
	awful.button({ }, 4, awful.tag.viewprev),
	awful.button({ }, 5, awful.tag.viewnext)
	))

-- f]]

-- Client Keys/Buttons:   f[[
clientkeys = awful.util.table.join(
	awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle ),
	awful.key({ modkey, "Control" }, "Return", function(c) c:swap(awful.client.getmaster()) end),
	awful.key({ modkey, }, "o",    awful.client.movetoscreen ),
	awful.key({ modkey, }, "s",    function(c) c.sticky = not c.sticky end),

	awful.key({ },         "F11",  function(c) c.fullscreen = not c.fullscreen  end),
	awful.key({ modkey, }, "F4",   function(c) c:kill()                         end),
	awful.key({ altkey, }, "F4",   function(c) c:kill()                         end),
	awful.key({ altkey, }, "F12",  function(c) c.above = not c.above            end),
	awful.key({ altkey, }, "F9",   function(c) c.minimized = true end),
	awful.key({ altkey, }, "F10",  function(c)
	 c.maximized_horizontal = not c.maximized_horizontal
	 c.maximized_vertical   = not c.maximized_vertical
 end)
)
clientbuttons = awful.util.table.join(
	awful.button({ }, 1, function(c) client.focus = c; c:raise() end),
	awful.button({ modkey }, 1, awful.mouse.client.move),
	awful.button({ modkey }, 3, function(c)
			  c.maximized_horizontal = false
			  c.maximized_vertical = false
			  awful.mouse.client.resize(c, "bottom_right")
		  end))
-- f]]

-- Text Edit Keys
text_edit_key = awful.util.table.join(
	awful.key({altkey}, 'f',         function(c) exec('xdotool key --clearmodifiers --window ' .. c.window .. ' ctrl+Right'     ) end),
	awful.key({altkey}, 'b',         function(c) exec('xdotool key --clearmodifiers --window ' .. c.window .. ' ctrl+Left'      ) end),
	awful.key({'Control'}, 'd',      function(c) exec('xdotool key --clearmodifiers --window ' .. c.window .. ' Home'      ) end),
	awful.key({'Control'}, 'e',      function(c) exec('xdotool key --clearmodifiers --window ' .. c.window .. ' End'       ) end)
	)
function bind_text_key(client) client:keys(awful.util.table.join(client:keys(), text_edit_key)) end


awful.rules.rules = {
	{ rule = { },     -- default
	properties = {
		border_width = beautiful.border_width,
		border_color = beautiful.border_normal,
		focus = true,
		keys = clientkeys,
		buttons = clientbuttons,
	}
}, {
	rule = { class = "Screenruler" },
	properties = {
		floating = true,
		focus = false,
		border_width = 0,
	}
}, {
	rule = { name = "htop" },
	properties = {
		maximized_horizontal = true,
		maximized_vertical = true,
	}
}, {
	rule = { class = "Chromium" },
	properties = { floating = true, },
}, {
	rule = { class = "Wireshark", name = "Wireshark" }, -- wireshark startup window
	properties = { floating = true }
}, {
	rule_any = {
		instance = {'TM.exe', 'QQ.exe'},
	},
	properties = {
		-- This, together with myfocus_filter, make the popup menus flicker taskbars less
		-- Non-focusable menus may cause TM2013preview1 to not highlight menu items on hover and crash.
		focusable = true,
		floating = true,
		border_width = 0,
	}
}, {
	rule_any = {
		class = {
			'MPlayer', 'feh', 'Display', 'Gimp',
			'Screenkey', 'Dia', 'Pavucontrol', 'Stardict', 'XEyes', 'Skype',
		},
		name = {
			'文件传输', 'Firefox 首选项', '暂存器', 'Keyboard', tmp_terminal_name
		},
		instance = {
			'Browser', -- 火狐的关于对话框
			'MATLAB', -- splash
		},
	},
	properties = { floating = true, }
}, {
	rule_any = {
		class = {'MPlayer', 'feh'},
	},
	properties = { above = true, }
} }

client.connect_signal(
	"manage",
	function(c, startup)
		-- Enable sloppy focus
		c:connect_signal("mouse::leave",
			 function(c)
				 if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier then
					 client_unfocused = c.window
				 end
			 end)

		c:connect_signal("mouse::enter", function(c)
			 -- 如果离开后又进入同一窗口则忽略，这解决了由于输入条而造成的焦点移动
			 if client_unfocused ~= c.window and awful.layout.get(c.screen) ~= awful.layout.suit.magnifier then
				 client.focus = c
			 end
		 end)

		if not startup then
			-- Set the windows at the slave,
			-- i.e. put it at the end of others instead of setting it master.
			-- awful.client.setslave(c)

			-- Put windows in a smart way, only if they does not set an initial position.
			if not c.size_hints.user_position and not c.size_hints.program_position then
				awful.placement.no_overlap(c)
				awful.placement.no_offscreen(c)
			end
		end

		if c.instance == "TM.exe" then
			bind_text_key(c)
		end
	end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
exec("awesomeup", false)
