
local myutil = {
exec              = awful.util.spawn,
sexec             = awful.util.spawn_with_shell,
exec_sync         = awful.util.pread,
join              = awful.util.table.join,
}

-- TODO use naughty
function myutil.notify(title, text, urgency)	-- normal, low, critical
	title = tostring(title)
	text = tostring(text)
	if not urgency then urgency = 'normal' end
	myutil.exec('notify-send -u ' .. urgency .. ' "'.. title .. '" "'.. text .. '"')
end

function myutil.run_term(cmd, name)
   local TMP_TERM = 'TODO'
   if not name then name = TMP_TERM end
	myutil.exec("urxvt -name '" .. name .. "' -e bash -c 'source $HOME/.bashrc; " .. cmd .. "'")
end

function myutil.net_monitor()
    myutil.run_term('tmux new-session -d "sudo iftop -i "' .. active_net_if .. ' \\; split-window -d "sudo nethogs ' .. active_net_if .. '" \\; attach',
             'FSTerm')
end

function myutil.sendkey(c, key)		-- send key in xdotool format
    myutil.exec_sync('sleep 0.1')
    myutil.exec('xdotool key --clearmodifiers ' .. key)
end

function myutil.rexec(cmd)
    --return awful.util.pread(cmd)
    local f = io.popen(cmd)
    local ret = f:read('*all')
    f:close()
    return ret
end

function myutil.moveresize_abs(x, y, w, h, c)
   -- set the pos/size of a client
   -- x, y: should be in pixel, can be negative (couting from right or bottom)
   -- w, h: can be in [0,1] for ratio or in pixel, or 0 to keep unchanged
	c.maximized_horizontal = false
	c.maximized_vertical = false
	local g = c:geometry()
   local scr = screen[c.screen].workarea
	if w == 0 then w = g.width end
   if h == 0 then h = g.width end
	if w <= 1 then w = scr.width * w end
   if h <= 1 then h = scr.height * h end
   if math.abs(x) <= 1 then x = scr.width * x end
   if math.abs(y) <= 1 then y = scr.height * y end
   if x < 0 then x = scr.width + x end
   if y < 0 then y = scr.height + y end
	awful.client.moveresize(-g.x + scr.x + x, -g.y + scr.y + y,
						-g.width + w, -g.height + h, c)
end

-- string split by sep
function myutil.split(str, sep)
   local result = {}
   local regex = ("([^%s]+)"):format(sep)
   for each in str:gmatch(regex) do
      table.insert(result, each)
   end
   return result
end


return myutil
