local myutil = require('lib/myutil')
local const = require('rc/const')
local modkey = const.modkey

-- register key "mod + key" to tag "index"
local function register_tagkey(key, index)
	ROOT_KEYS = myutil.join(
		ROOT_KEYS,
		awful.key({modkey }, key, function()      -- view only
			local screen = mouse.screen
			if tags[screen][index] then
				awful.tag.viewonly(tags[screen][index])
			end
		end),
	awful.key({modkey, "Control" }, key, function()    -- toggle view
		   local screen = mouse.screen
		   if tags[screen][index] then
			   awful.tag.viewtoggle(tags[screen][index])
		   end
	   end),
	awful.key({modkey, "Shift"   }, key, function()        -- move but not jump
         local screen = client.focus.screen
		   if client.focus and tags[screen][index] then
			   awful.client.movetotag(tags[screen][index])
		   end
	   end),
	awful.key({modkey, "Control", "Shift" }, key, function()  -- add to new tag
         local screen = client.focus.screen
		   if client.focus and tags[screen][index] then
			   awful.client.toggletag(tags[screen][index])
		   end
	   end)
	)
end


local tag_name = { "1", "2", "chat", "0"}
local tags = {}   -- tags: screen -> table of tags
awful.screen.connect_for_each_screen(function(s)
    tags[s] = awful.tag(tag_name, s, const.default_layout)
end)

-- because 0 is on the right of keyboard
for key = 1, #tag_name - 1 do
   register_tagkey(key, key)
end
register_tagkey(0, #tag_name)

ROOT_KEYS = awful.util.table.join(
  ROOT_KEYS,
	awful.key({modkey}, "Left",   awful.tag.viewprev       ),
	awful.key({modkey}, "Right",  awful.tag.viewnext       ),
	awful.key({modkey}, "Escape", awful.tag.history.restore)
)

return tags
