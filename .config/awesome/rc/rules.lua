-- Text Edit Keys
local myutil = require('lib/myutil')
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

awful.rules.rules = {
{ rule = { },     -- default
	properties = {
		border_width = beautiful.border_width,
		border_color = beautiful.border_normal,
		focus = true,
		keys = keys.client_keys,
		buttons = keys.client_buttons,
    placement = awful.placement.no_overlap+awful.placement.no_offscreen,
    screen = awful.screen.preferred,
	}},

{ rule = { class = "Chromium" },
  properties = {
    keys = awful.util.table.join(
      keys.client_keys,
      awful.key({'Control'}, 'q', function(c)
        myutil.rexec('sleep 0.3')
        awful.key.execute({}, "Tab")
        awful.key.execute({}, "Escape")
        awful.key.execute({"Control"}, "w")
      end))
  }},

--[[
   [{ rule = { instance = 'FSTerm' },
	 [  properties = {
	 [    maximized_horizontal = true,
	 [    maximized_vertical = true,
	 [  } },
   ]]

-- floating:
{ rule_any = {
		class = { 'MPlayer', 'feh', 'Screenkey', 'Skype' },
		name = { '文件传输', 'Firefox 首选项', '暂存器', 'Keyboard',
         --TMP_TERM
		},
	},
	properties = { floating = true, }
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
  end}
} -- the end

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
			 if client_unfocused ~= c.window and awful.layout.get(c.screen) ~= awful.layout.suit.magnifier then
				 client.focus = c
			 end
		 end)

		if startup then
         awful.rules.apply(c)
		end
	end)

client.connect_signal("focus", function(c)
  c.border_color = beautiful.border_focus
end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
