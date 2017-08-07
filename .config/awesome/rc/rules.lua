-- Text Edit Keys
local myutil = require('lib/myutil')
local wibox = require("wibox")
local gears = require('gears')
--[[
   [local text_edit_key = myutil.join(
	 [  awful.key({altkey}, 'f',         function(c) sendkey(c, 'ctrl+Right') end),
	 [  awful.key({altkey}, 'b',         function(c) sendkey(c, 'ctrl+Left') end),
	 [  awful.key({'Control'}, 'd',      function(c) sendkey(c, 'Home') end),
	 [  awful.key({'Control'}, 'e',      function(c) sendkey(c, 'End') end)
   [)
   [local function bind_text_key(client)
	 [  client:keys(myutil.join(client:keys(), text_edit_key))
   [end
   ]]
local print_client = function(c)
  myutil.notify(c.name)
end

awful.rules.rules = {
{ rule = { },     -- default
	properties = {
		border_width = beautiful.border_width,
		border_color = beautiful.border_normal,
		focus = true,
		keys = keys.client_keys,
		buttons = keys.client_buttons,
    placement = awful.placement.no_overlap+awful.placement.no_offscreen,
    titlebars_enabled = true,
    screen = awful.screen.preferred,
	}},

{ rule = { class = "Chromium" },
  properties = {
    keys = gears.table.join(
      keys.client_keys,
      awful.key({'Control'}, 'q', function(c)
        awful.spawn.easy_async(
          'sleep 0.3',
          function(...)
            awful.key.execute({}, "Tab")
            awful.key.execute({}, "Escape")
            awful.key.execute({"Control"}, "w")
          end)
      end))
  }},

-- floating:
{ rule_any = {
		class = { 'MPlayer', 'feh', 'Screenkey', 'Skype' },
		name = { '文件传输', 'Firefox 首选项', '暂存器', 'Keyboard' },
	},
	properties = { floating = true, },
},

-- always above:
{ rule_any = {
		class = {'MPlayer', 'feh'},
	},
	properties = { above = true, }
},

{ rule = {instance = 'gimp'},
  properties = { border_width = 3 },
  -- bring up all other gimp
  callback = function(c)
    for _, j in ipairs(client.get()) do
      if j.instance == 'gimp' then
        j:raise()
      end
    end
  end
},

-- chat:
{ rule_any = { name = {'Telegram', 'plaidchat', 'WeChat', 'Nocturn'} },
  properties = { tag = "chat" },
  callback = function(c)
    local g = c:geometry()
    if c.name == "Nocturn" then
       myutil.moveresize_abs(-400, 0, 400, 1, c)
    elseif c.name:find("WeChat") ~= nil then
       myutil.moveresize_abs(0, 0, 900, 0.8, c)
    elseif c.name:find("Telegram") ~= nil then
       myutil.moveresize_abs(-1200, -800, 1000, 800, c)
    end
  end},

  -- for the vizdoom demo @ICLR17
  --[[
     [{ rule = { class = "vizdoom" },
     [  callback = function(c)
     [    if c.width == 1024 then
     [      myutil.moveresize_abs(-1024, -768, 1024, 768, c)
     [    else
     [      for _, cc in ipairs(mouse.screen.clients) do
     [        if cc ~= c and cc.width == 512 and cc.y ~= 384 then
     [          myutil.moveresize_abs(0, 384, 512, 384, c)
     [          return
     [        end
     [      end
     [    end
     [  end
     [},
     ]]
} -- the end

client.connect_signal("unmanage", function(c)
  awful.client.focus.history.delete(c)
end)

-- sloppy focus
client.connect_signal("mouse::enter", function(c)
  if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier then
    client.focus = c
  end
  local s = c.screen
  s.my_systray:set_screen(s)  -- dyanmic systray location
end)

client.connect_signal("focus", function(c)
  c.border_color = beautiful.border_focus
end)
client.connect_signal("unfocus", function(c)
  c.border_color = beautiful.border_normal
end)

-- Title bar f[[
local titlebar_buttons = gears.table.join(
  awful.button({ }, 1, function()
    client.focus = c
    c:raise()
    awful.mouse.client.move(c)
  end),
  awful.button({ }, 3, function()
    client.focus = c
    c:raise()
    awful.mouse.client.resize(c)
  end)
)

client.connect_signal("request::titlebars", function(c)
  awful.titlebar(c, {size = 20}):setup {
    { -- Left
      awful.titlebar.widget.iconwidget(c),
      buttons = titlebar_buttons,
      layout  = wibox.layout.fixed.horizontal
    },
    { -- Middle
      { -- Title
        align  = "center",
        widget = awful.titlebar.widget.titlewidget(c)
      },
      buttons = titlebar_buttons,
      layout  = wibox.layout.flex.horizontal
    },
    { -- Right
      awful.titlebar.widget.floatingbutton (c),
      awful.titlebar.widget.maximizedbutton(c),
      awful.titlebar.widget.stickybutton   (c),
      awful.titlebar.widget.ontopbutton    (c),
      awful.titlebar.widget.closebutton    (c),
      layout = wibox.layout.fixed.horizontal()
    },
    layout = wibox.layout.align.horizontal
  }
  awful.titlebar.hide(c)
end)
-- f]]
