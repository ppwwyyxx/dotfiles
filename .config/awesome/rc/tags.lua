
-- Tags
-- Define a tag table which hold all screen tags.

local tag_name = { "1", "2", "chat", "0"}
last_tag = #tag_name
tags = {}   -- tags: screen -> tags

awful.screen.connect_for_each_screen(function(s)
    tags[s] = awful.tag(tag_name, s, awful.layout.suit.floating)
end)

local function register_tagkey(key, index)
	config.global = join(
		config.global,
		awful.key({ modkey }, key, function()      -- view only
			local screen = mouse.screen
			if tags[screen][index] then
				awful.tag.viewonly(tags[screen][index])
			end
		end),
	awful.key({ modkey, "Control" }, key, function()    -- toggle view
		   local screen = mouse.screen
		   if tags[screen][index] then
			   awful.tag.viewtoggle(tags[screen][index])
		   end
	   end),
	awful.key({ modkey, "Shift"   }, key, function()        -- move but not jump
         local screen = client.focus.screen
		   if client.focus and tags[screen][index] then
			   awful.client.movetotag(tags[screen][index])
		   end
	   end),
	awful.key({ modkey, "Control", "Shift" }, key, function()
         local screen = client.focus.screen
		   if client.focus and tags[screen][index] then
			   awful.client.toggletag(tags[screen][index])
		   end
	   end)
	)
end

local keynumber = last_tag - 1
for key = 1, keynumber do
   register_tagkey(key, key)
end
register_tagkey(0, last_tag)
