-- Text Edit Keys
local run_or_raise = require("lib/run_or_raise")
local text_edit_key = join(
	awful.key({altkey}, 'f',         function(c) sendkey(c, 'ctrl+Right') end),
	awful.key({altkey}, 'b',         function(c) sendkey(c, 'ctrl+Left') end),
	awful.key({'Control'}, 'd',      function(c) sendkey(c, 'Home') end),
	awful.key({'Control'}, 'e',      function(c) sendkey(c, 'End') end)
)
local function bind_text_key(client)
	client:keys(join(client:keys(), text_edit_key))
end

local ad_blocked = 0

awful.rules.rules = {
{ rule = { },     -- default
	properties = {
		border_width = beautiful.border_width,
		border_color = beautiful.border_normal,
		focus = true,
		keys = config.clientkeys,
		buttons = config.clientbuttons,
      placement = awful.placement.no_overlap+awful.placement.no_offscreen,
	}
}, {
    rule = { class = "Chromium" },
    callback = function(c)
        c:keys(join(c:keys(),
            awful.key({'Control'}, 'q', function(c)
                      exec_sync('sleep 0.3')
                      sendkey(c, 'Tab Escape Ctrl+w')
                  end)
        ))
    end
}, {
	rule = { instance = 'FSTerm' },
	properties = {
		maximized_horizontal = true,
		maximized_vertical = true,
	}
}, {
	rule = { instance = 'EMAIL' },
	properties = {
		maximized_horizontal = true,
		maximized_vertical = true,
	},
    callback = function(c)
        awful.client.movetotag(tags[screen.count()][3], c)
    end
}, {
	rule_any = {
		instance = {'TM.exe', 'QQ.exe'},
	},
	properties = {
		focusable = false,
		floating = true,
		border_width = 0,
	},
    callback = function(c)
        bind_text_key(c)
        if c.name and c.name:match('^腾讯') and c.above then
            ad_blocked = ad_blocked + 1
            notify("Ad Blocked " .. ad_blocked, "One window blocked, title: ".. c.name)
            c:kill()
        end
    end
}, {
	rule_any = {
		class = {
			'MPlayer', 'feh', 'Screenkey', 'Skype',
		},
		name = {
			'文件传输', 'Firefox 首选项', '暂存器', 'Keyboard', TMP_TERM
		},
	},
	properties = { floating = true, }
}, {
	rule_any = {
		class = {'MPlayer', 'feh'},
	},
	properties = { above = true, }
}, {
    rule = {instance = 'gimp'},
    properties = { border_width = 3 }
}, {
    rule = { class = 'rdesktop'},
    properties = { screen = 1 }
}, {
    rule_any = { name = {'Telegram', 'plaidchat', 'WeChat', 'Nocturn'} },
    callback = function(c)
        awful.client.movetotag(tags[screen[1]][3], c)
        local g = c:geometry()
        if c.name == "Nocturn" then
           moveresize_abs(-400, 0, 400, 1, c)
        elseif c.name:find("WeChat") ~= nil then
           moveresize_abs(0, 0, 900, 0.8, c)
        elseif c.name:find("Telegram") ~= nil then
           moveresize_abs(-1200, -800, 1000, 800, c)
        end
    end
}
}

client.connect_signal(
	"manage",
	function(c, startup)
        --[[
           [if c.name and c.name:match('新词锐词') then
           [    ad_blocked = ad_blocked + 1
           [    notify("Ad Blocked " .. ad_blocked .. c.name)
           [    c:kill()
           [end
           ]]
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
      if c.instance == 'gimp' then
          local curtags = awful.tag.selectedlist()
          for _, curtag in ipairs(curtags) do
              for _, j in ipairs(curtag:clients()) do
                  if j.instance == 'gimp' then
                      j:raise()
                  end
              end
          end
      end
  end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
