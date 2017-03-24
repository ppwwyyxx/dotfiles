local myutil = require('lib/myutil')
local const = require('rc/const')
local modkey = const.modkey
local altkey = const.altkey
local mouse_control = require("lib/mouse")
local sdcv_selection = require("lib/sdcv")
local web_cmd = require("lib/web_cmd")

ROOT_KEYS = awful.util.table.join(
	ROOT_KEYS,
	awful.key({modkey}, "n", function() awful.screen.focus_relative(1) end),
	awful.key({modkey}, "u", awful.client.urgent.jumpto),

	-- Layout manipulation for tiling
	awful.key({modkey}, "space", function() awful.layout.inc(const.available_layouts,  1) end),
	awful.key({modkey, "Shift"}, "space", function() awful.layout.inc(const.available_layouts, -1) end),
	awful.key({modkey}, "l",     function() awful.tag.incmwfact( 0.05)    end),
	awful.key({modkey}, "h",     function() awful.tag.incmwfact(-0.05)    end),

  -- snap window around
	awful.key({modkey, }, "'", function() myutil.moveresize_abs(-0.5, 0, 0.5, 1) end),
	awful.key({modkey, }, ";", function() myutil.moveresize_abs(0, 0, 0.5, 1) end),


  -- page up/down to resize
	awful.key({modkey}, "Next",  function ()
    if client.focus then client.focus:relative_move( 20,  20, -40, -40) end
  end),
  awful.key({modkey}, "Prior", function ()
    if client.focus then client.focus:relative_move(-20, -20,  40,  40) end
  end),

  -- interactive move & resize
	awful.key({modkey, "Shift"}, "'", function()
		keygrabber.run(function(mod, key, event)
		     if event == "release" then return end
         local c = client.focus
         if not c then return end
		     if     key == 'k'    then c:relative_move(0, -10, 0, 0)
		     elseif key == 'j'    then c:relative_move(0, 10, 0, 0)
		     elseif key == 'l'    then c:relative_move(10, 0, 0, 0)
		     elseif key == 'h'    then c:relative_move(-10, 0, 0, 0)
		     elseif key == ','    then c:relative_move(-30, 0, 0, 0)
		     elseif key == '.'    then c:relative_move(30, 0, 0, 0)
		     elseif key == 'Up'   then c:relative_move(0, -20, 0, 40)
		     elseif key == 'Down' then c:relative_move(0, 10, 0, -20)
		     elseif key == 'Right'then c:relative_move(-20, 0, 40, 0)
		     elseif key == 'Left' then c:relative_move(10, 0, -20, 0)
		     else keygrabber.stop() end
		end)
	end),

  -- all kinds of terminal
  awful.key({modkey, "Shift"}, "t", function()
      keygrabber.run(function(mod, key, event)
          if event == 'release' then return
          elseif key == 'Return' then myutil.exec(const.terminal)
          elseif key == 'p' then myutil.run_term('bpython')
          elseif key == 'P' then myutil.run_term('bpython2')
          elseif key == 'c' then myutil.run_term('coffee')
          elseif key == 't' then myutil.run_term('top', 'FSTerm')
          elseif key == 'h' then myutil.run_term('htop', 'FSTerm')
          elseif key == 'd' then myutil.run_term('dstat -dnmcl --top-io -Nwlp3s0', 'FSTerm')
          elseif key == 'Shift_L' or key == 'Shift_R' then return
          end
          keygrabber.stop()
      end)
  end),

  -- move mouse with keyboard
  awful.key({modkey }, "m", mouse_control),

	-- big things
	awful.key({ altkey, "Control", "Shift"}, "r", function()
            check = myutil.rexec("awesome -k 2>&1")
            if string.find(check, 'syntax OK') then
                awesome.restart()
            else
                myutil.notify("Conf Syntax Error!", check)
            end
  end),

  awful.key({altkey, "Control", 'Shift'}, 'd', function()
    -- debug operation here
    myutil.notify("debug")
  end),

	-- switch clients order
  awful.key({modkey, "Shift"}, "j", function() awful.client.swap.byidx(1) end),
  awful.key({modkey, "Shift"}, "k", function() awful.client.swap.byidx(-1) end),

  -- switch focused clients
	awful.key({modkey}, "j", function()
        awful.client.focus.byidx(1)
        if client.focus then client.focus:raise() end
  end),
  awful.key({modkey}, "k", function()
      awful.client.focus.byidx(-1)
      if client.focus then client.focus:raise() end
  end),

  -- alt-tab switch
  -- better use a queue to implement this
	awful.key({altkey}, "Tab", function()
        local nowc = client.focus
        awful.client.focus.history.previous()
        if client.focus then client.focus:raise() end
        keygrabber.run(function(mod, key, event)
            if event == 'release' then
              if key == 'Alt_L' then
                  if nowc ~= client.focus then
                      awful.client.focus.history.add(nowc)
                      awful.client.focus.history.add(client.focus)
                  end
                  keygrabber.stop()
              end
            else
              if key == 'Tab' then
                awful.client.focus.byidx(1)
                if client.focus then client.focus:raise() end
              end
            end
       end)
	end),

  -- toggle show desktop
	awful.key({modkey}, "d", function()
		local curtags = awful.tag.selectedlist()
    local all_minimized = function()
      for _, curtag in ipairs(curtags) do
        for _, c in ipairs(curtag:clients()) do
          if not c.minimized then
            return false
          end
        end
      end
      return true
    end
    all_minimized = all_minimized()

    for _, curtag in ipairs(curtags) do
      for _, c in ipairs(curtag:clients()) do
        if not all_minimized then
          c.minimized = true
        else
          c.minimized = false
          client.focus = c
          c:raise()
        end
      end
    end
	end),

	-- Common program
	awful.key({modkey}, "q", function()
		local c = client.focus
		if not c then return end
      --[[
			[if c.instance == 'FSTerm' or c.instance == TMP_TERM then
			[   awful.client.movetotag(tags[c.screen][last_tag], c)
         [  else
         ]]
    c:kill()
      --[[
			[end
         ]]
	end),

	-- sdcv
	awful.key({altkey}, "F3", sdcv_selection),


  -- yubnub. g;wp;gfl;gi;gm;yt;py;python(search);pypi;rdoc;cppdoc;dbm
  awful.key({ modkey }, "w", function()
     local sc = awful.screen.focused()
     awful.prompt.run {
       prompt = "Web: ",
       textbox = sc.my_prompt_box.widget,
       exe_callback = function(command)
         local url = web_cmd(command)
         myutil.sexec(const.browser .. '"' .. url .. '"')
       end}
  end),

	-- Volume
	awful.key({}, 'XF86AudioRaiseVolume', function() volumectl("up") end),
	awful.key({}, 'XF86AudioLowerVolume', function() volumectl("down") end),
	awful.key({}, 'XF86AudioMute', function() volumectl("mute") end)
)



local function toggle_maximize(c)
    c.maximized_horizontal = not c.maximized_horizontal
    c.maximized_vertical   = not c.maximized_vertical
end
-- Client Keys/Buttons:   f[[
local CLIENT_KEYS = awful.util.table.join(
	awful.key({modkey, "Control"}, "Return", function(c) c:swap(awful.client.getmaster()) end),
  awful.key({modkey}, "o", function(c)
      if screen.count() == 1 then return end
      c:move_to_screen()
  end),
	awful.key({modkey}, "s", function(c) c.sticky = not c.sticky end),

	awful.key({altkey}, "F11",  function(c) c.fullscreen = not c.fullscreen  end),
	awful.key({altkey}, "F4",   function(c) c:kill()                         end),
	awful.key({altkey}, "F12",  function(c) c.above = not c.above            end),
	awful.key({altkey}, "F9",   function(c) c.minimized = true end),
  awful.key({altkey}, "F10",  toggle_maximize),
  awful.key({modkey}, "Up",  toggle_maximize)
)

local CLIENT_BUTTONS = awful.util.table.join(
	awful.button({}, 1, function(c) client.focus = c; c:raise() end),
	awful.button({modkey}, 1, awful.mouse.client.move),
  awful.button({modkey}, 3, function(c)
     c.maximized_horizontal = false
     c.maximized_vertical = false
     awful.mouse.client.resize(c, "bottom_right")
  end))
-- f]]

return {
  client_keys = CLIENT_KEYS,
  client_buttons = CLIENT_BUTTONS
}
