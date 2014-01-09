
exec              = awful.util.spawn
sexec             = awful.util.spawn_with_shell
join              = awful.util.table.join


function notify(title, text, urgency)	-- normal, low, critical
	title = tostring(title)
	text = tostring(text)
	if not urgency then urgency = 'normal' end
	exec('notify-send -u ' .. urgency .. ' "'.. title .. '" "'.. text .. '"')
end

function run_term(cmd, name)
	exec(terminal .. " -name '" .. name .. "' -e bash -c '" .. cmd .. "'")
end

function sendkey(c, key)		-- send key in xdotool format
    if not c then
	    exec('xdotool key --clearmodifiers ' .. key)
    else
	    exec('xdotool key --clearmodifiers --window ' .. c.window .. ' ' .. key)
    end
end

function rexec(cmd)
    return awful.util.pread(cmd)
    --[[
	   [local f = io.popen(cmd)
	   [local ret = f:read('*all')
	   [f:close()
	   [return ret
       ]]
end
