local lgi = require('lgi')
local Gio = lgi.Gio
local gears = require('gears')
local const = require('rc/const')
local myutil = {
exec              = awful.spawn,
join              = gears.table.join,
}

-- string split by sep
function myutil.split(str, sep)
   local result = {}
   local regex = ("([^%s]+)"):format(sep)
   for each in str:gmatch(regex) do
      table.insert(result, each)
   end
   return result
end

-- http://lua-users.org/wiki/StringTrim
local match = string.match
function myutil.trim(s)
  return match(s,'^()%s*$') and '' or match(s,'^%s*(.*%S)')
end


function myutil.colored_text(text, color, extra_attr)
   extra_attr = extra_attr or ""
   if not color then  -- having span is better for appearance
     return string.format("<span %s>%s</span>", extra_attr, text)
   end
   return string.format(
      "<span foreground=\"%s\" %s>%s</span>", color, extra_attr, text)
end


-- TODO use naughty
function myutil.notify(title, text, urgency)	-- normal, low, critical
	title = tostring(title)
	text = tostring(text)
	if not urgency then urgency = 'normal' end
	myutil.exec('notify-send -u ' .. urgency .. ' "'.. title .. '" "'.. text .. '"')
end

function myutil.run_term(cmd, name)
   if not name then name = cmd end
   name = name .. const.TMP_TERM
   cmd = "urxvt -name '" .. name .. "' -e bash -c 'source $HOME/.bashrc; " .. cmd .. "'"
   local matcher = function(c)
     return (c.instance == name)
   end
   awful.client.run_or_raise(cmd, matcher)
end

function myutil.rexec(cmd)
  return Gio.Async.call(function()
    local f = io.popen(cmd)
    local ret = f:read('*all')
    f:close()
    return ret
  end)()
end

function myutil.sendkey(c, key)		-- send key in xdotool format
  -- just use awful.key.execute
  awful.spawn.easy_async{
    cmd = 'sleep 0.1',
    callback = function()
      awful.spawn('xdotool key --clearmodifiers ' .. key)
    end}
end


function myutil.moveresize_abs(x, y, w, h, c)
  if not c then
    c = client.focus
  end
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
	c:relative_move(
    -g.x + scr.x + x, -g.y + scr.y + y,
    -g.width + w, -g.height + h)
end

function myutil.get_active_iface()
     local f = io.open('/proc/net/route')
     local netif
     for line in f:lines() do
         netif = line:match('^(%w+)%s+00000000%s')
         if netif then
             break
         end
     end
     f:close()
     return netif
end

function myutil.refresh_awesome()
  awesome.emit_signal("refresh")
end

function myutil.remove_by_value(tbl, val)
  local pos = awful.util.table.hasitem(tbl, val)
  if pos then
    table.remove(tbl, pos)
  end
  return pos
end

function myutil.move_clients_among_screen(src, dst)
  -- src, dst: both are screen
  if src.index == dst.index then return end
  for _, c in ipairs(src.all_clients) do
    c:move_to_screen(dst)
    local oldtags = c:tags()
    local newtags = {}
    for _, t in ipairs(oldtags) do
      table.insert(newtags, dst.tags[t.index])
    end
    c:tags(newtags)
  end
end

function myutil.find_largest_screen()
  local target = nil
  local area = 0
  for s in screen do
    local screen_area = s.geometry.width * s.geometry.height
    if screen_area > area then
      area = screen_area
      target = s
    end
  end
  return target
end

return myutil
