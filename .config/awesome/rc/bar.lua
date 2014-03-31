
local vicious = require("vicious")
local wibox = require("wibox")

-- Separators
local sepopen = wibox.widget.imagebox()
sepopen:set_image(beautiful.icons .. "/widgets/left.png")
local sepclose = wibox.widget.imagebox()
sepclose:set_image(beautiful.icons .. "/widgets/right.png")
local spacer = wibox.widget.imagebox()
spacer:set_image(beautiful.icons .. "/widgets/spacer.png")

-- Simple Widgets
local my_textclock = awful.widget.textclock("%m-%d %H:%M:%S %a", 1)
my_textclock:buttons(
    awful.button({}, 1, function() sexec(browser .. "http://calendar.google.com") end)
)

local cpugraph = awful.widget.graph()
cpugraph:set_width(40):set_height(16)
cpugraph:set_background_color("#00000033")
cpugraph:set_color({ type = "linear",
                   from = { 0, 0 }, to = { 10,0 },
                   stops = { {0, "#FF5656"}, {0.5, "#88A175"}, {1, "#AECF96" }}})
cpugraph:buttons(awful.button({}, 1, function() run_term('htop', 'FSTerm') end))
vicious.register(cpugraph, vicious.widgets.cpu, "$1")

local temp_widget = wibox.widget.textbox()
vicious.register(temp_widget, vicious.widgets.thermal, " $1°C", 19, "thermal_zone0")

local mem_widget = wibox.widget.textbox()
vicious.register(mem_widget, vicious.widgets.mem, '<span color="#90ee90"> M$1%</span>')
mem_widget:buttons(awful.button({}, 1, function() run_term('top -o %MEM -d 1', 'FSTerm') end))

-- Network f[[
local net_widget = wibox.widget.textbox()
local netgraph = awful.widget.graph()
netgraph:set_width(40):set_height(16)
netgraph:set_stack(true):set_scale(true)
netgraph:set_stack_colors({ "#c2ba62", "#5798d9" })
netgraph:set_background_color("#00000033")
vicious.register(net_widget, vicious.widgets.net, function(widget, args)
        local f = io.open('/proc/net/route')
        local netif
        for line in f:lines() do
            netif = line:match('^(%w+)%s+00000000%s')
            if netif then
                break
            end
        end
        f:close()
        active_net_if = netif

        local up, down, iface = 0, 0
        -- sum up/down value for all interfaces
        for name, value in pairs(args) do
           iface = name:match("^{(%S+) down_b}$")
           if iface == active_net_if then down = down + value end
           iface = name:match("^{(%S+) up_b}$")
           if iface == active_net_if then up = up + value end
        end
        netgraph:add_value(up, 1)
        netgraph:add_value(down, 2)
        local function format(val)
            -- no network
            if val > 500000000 then return "0" end
            if val > 500000 then return string.format("%.1fM", val/1000000.)
            else return string.format("%.0fK", val/1000.) end
        end
        return string.format('<span color="#5798d9">↓%s</span><span color="#c2ba62">↑%s</span>',
                       format(down), format(up))
    end, 5)
net_widget:buttons(
    awful.button({}, 1, net_monitor)
)
-- f]]

--Battery f[[
local bat_widget = wibox.widget.textbox()
vicious.register(bat_widget, vicious.widgets.bat, function(widget, args)
				local color = "#990099"
				if args[1] == '+' then color = "#00ffff"
				elseif args[1] == '−' then color = "#1e90ff" end

				local current = args[2]
				if current < 25 and args[1] == '−' then
					if current ~= bat_widget.lastwarn then
						notify("Low Battery", "Battery: " .. current .. "%.\n" .. args[3] .. " left.", 'critical')
						bat_widget.lastwarn = current
					end
				end
				return string.format('<span color="' .. color .. '">B%s%d%%</span>', args[1], current)
			end,
			59, "BAT0")
bat_widget:buttons(awful.button({}, 1, function() run_term("sudo powertop", 'FSTerm') end))
-- f]]

--Volume f[[
local volume_widget = wibox.widget.textbox()
function volumectl(mode)
    if mode == "update" then
        local volume = rexec("pamixer --get-volume")
        if not tonumber(volume) then
            volume_widget:set_markup("<span color='red'>ERR</span>")
            return
        end
		local muted = rexec("pamixer --get-mute")
        volume = '♫' .. volume .. ((muted == "false") and "%" or "<span color='red'>M</span>")
        volume_widget:set_markup(volume)
		return
    elseif mode == "up" then
        exec("pamixer --allow-boost --increase 5")
    elseif mode == "down" then
        exec("pamixer --allow-boost --decrease 5")
	elseif mode == "mute" then
        exec("pamixer --toggle-mute")
    end
	volumectl("update")
end
volumectl()
local volume_clock = timer({ timeout = 60 })
volume_clock:connect_signal("timeout", function() volumectl() end)
volume_clock:start()
volume_widget:buttons(join(
    awful.button({ }, 4, function() volumectl("up") end),
    awful.button({ }, 5, function() volumectl("down") end),
    awful.button({ }, 3, function() exec("pavucontrol") end),
    awful.button({ }, 1, function() volumectl("mute") end)
))
-- f]]


-- tag, task

local my_taglist = {}
my_taglist.buttons = join(
    awful.button({ }, 1, awful.tag.viewonly),
    awful.button({ modkey }, 1, awful.client.movetotag),
    awful.button({ }, 3, awful.tag.viewtoggle),
    awful.button({ modkey }, 3, awful.client.toggletag),
    awful.button({ }, 4, function(t) awful.tag.viewnext(awful.tag.getscreen(t)) end),
    awful.button({ }, 5, function(t) awful.tag.viewprev(awful.tag.getscreen(t)) end)
)

local task_list = {}
local instance
task_list.buttons = join(
	awful.button({ }, 1, function(c)
			  if c == client.focus and not c.minimized then
				  c.minimized = true
			  else
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

local my_wibox = {}
my_promptbox = {}
local my_layoutbox = {}
for s = 1, screen.count() do
    my_layoutbox[s] = awful.widget.layoutbox(s)
    my_layoutbox[s]:buttons(join(
        awful.button({ }, 1, function() awful.layout.inc(config.layouts, 1) end),
        awful.button({ }, 3, function() awful.layout.inc(config.layouts, -1) end),
        awful.button({ }, 4, function() awful.layout.inc(config.layouts, 1) end),
        awful.button({ }, 5, function() awful.layout.inc(config.layouts, -1) end)
    ))
    my_taglist[s] = awful.widget.taglist(s, awful.widget.taglist.filter.all, my_taglist.buttons)
    task_list[s] = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, task_list.buttons)
    my_wibox[s] = awful.wibox({ position = "top", screen = s, height = 20 })
    my_promptbox[s] = awful.widget.prompt()

    local left_layout = wibox.layout.fixed.horizontal()
    left_layout:add(sepopen)
    left_layout:add(my_taglist[s])
    left_layout:add(my_promptbox[s])
    left_layout:add(sepclose)

    local right_layout = wibox.layout.fixed.horizontal()
    right_layout:add(sepopen)
    right_layout:add(cpugraph)
    right_layout:add(temp_widget)
    right_layout:add(mem_widget)
    right_layout:add(bat_widget)
    --right_layout:add(netgraph)
    right_layout:add(net_widget)
    right_layout:add(volume_widget)
    if s == 1 then
        right_layout:add(wibox.widget.systray())
    end
    right_layout:add(sepclose)
    right_layout:add(my_textclock)
    right_layout:add(my_layoutbox[s])

    local layout = wibox.layout.align.horizontal()
    layout:set_left(left_layout)
    layout:set_middle(task_list[s])
    layout:set_right(right_layout)

    my_wibox[s]:set_widget(layout)
end
