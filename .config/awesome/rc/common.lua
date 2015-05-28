
exec              = awful.util.spawn
sexec             = awful.util.spawn_with_shell
exec_sync         = awful.util.pread
join              = awful.util.table.join


function notify(title, text, urgency)	-- normal, low, critical
	title = tostring(title)
	text = tostring(text)
	if not urgency then urgency = 'normal' end
	exec('notify-send -u ' .. urgency .. ' "'.. title .. '" "'.. text .. '"')
end

function run_term(cmd, name)
    if not name then name = TMP_TERM end
	exec(terminal .. " -name '" .. name .. "' -e bash -c 'source $HOME/.bashrc; " .. cmd .. "'")
end

function net_monitor()
    run_term('tmux new-session -d "sudo iftop -i "' .. active_net_if .. ' \\; split-window -d "sudo nethogs ' .. active_net_if .. '" \\; attach',
             'FSTerm')
end

function sendkey(c, key)		-- send key in xdotool format
    exec_sync('sleep 0.1')
    exec('xdotool key --clearmodifiers ' .. key)
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
