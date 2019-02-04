local myutil = require('lib/myutil')
local gears = require('gears')
local const = require('rc/const')
local modkey = const.modkey

-- register key "mod + key" to tag "index"
local function register_tagkey(key, index)
	ROOT_KEYS = gears.table.join(
		ROOT_KEYS,
		awful.key({modkey}, key, function()      -- view only
			local screen = awful.screen.focused()
			if tags[screen][index] then
				tags[screen][index]:view_only()
			end
		end),
	awful.key({modkey, "Control" }, key, function()    -- toggle view
		   local screen = awful.screen.focused()
		   if tags[screen][index] then
			   awful.tag.viewtoggle(tags[screen][index])
		   end
	   end),
	awful.key({modkey, "Shift"}, key, function()        -- move but not jump
       local screen = awful.screen.focused()
       local cur_tag = tags[screen][index]
		   if client.focus and cur_tag then
         client.focus:move_to_tag(cur_tag)
		   end
	   end),
	awful.key({modkey, "Control", "Shift" }, key, function()  -- add to new tag
       local screen = awful.screen.focused()
		   if client.focus and tags[screen][index] then
			   client.focus:toggle_tag(tags[screen][index])
		   end
	   end)
	)
end


local tag_name = { "1", "2", "chat", "0"}
local tags = {}   -- tags: screen -> table of tags

local function handle_orphan_tag(t)
  -- called when the tag t has no screen

  -- Find a live screen
  local live_screen = nil;
  for s in screen do
      if s ~= t.screen then
          live_screen = s;
          break
      end
  end
  myutil.move_clients_among_screen(t.screen, live_screen)
end

local function put_clients_on_largest_screen()
  local target = nil
  local area = 0
  for s in screen do
    local screen_area = s.geometry.width * s.geometry.height
    if screen_area > area then
      area = screen_area
      target = s
    end
  end

  for s in screen do
    if s.index ~= target.index then
      myutil.move_clients_among_screen(s, target)
    end
  end
end

awful.screen.connect_for_each_screen(function(s)
    tags[s] = awful.tag(tag_name, s, const.default_layout)
    for i, tag in pairs(s.tags) do
      tag:connect_signal("request::screen", handle_orphan_tag)
    end
    if s.index ~= 1 then
      put_clients_on_largest_screen()
    end
end)

-- because 0 is on the right of keyboard
for key = 1, #tag_name - 1 do
   register_tagkey(key, key)
end
register_tagkey(0, #tag_name)

ROOT_KEYS = gears.table.join(
  ROOT_KEYS,
	awful.key({modkey}, "Left",   awful.tag.viewprev       ),
	awful.key({modkey}, "Right",  awful.tag.viewnext       ),
	awful.key({modkey}, "Escape", awful.tag.history.restore)
)

return tags
