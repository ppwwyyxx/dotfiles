
-- Tags
-- Define a tag table which hold all screen tags.
local tag_name = { "1", "2", "3", "4", "vim", "0"}
vim_tag = 5
tags, revtags = {}, {}
for s = 1, screen.count() do
    tags[s] = awful.tag(tag_name, s, awful.layout.suit.floating)
    for i, t in ipairs(tags[s]) do
        revtags[t] = i
    end
end

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
		   if client.focus and tags[client.focus.screen][index] then
			   awful.client.movetotag(tags[client.focus.screen][index])
		   end
	   end),
	awful.key({ modkey, "Control", "Shift" }, key, function()
		   if client.focus and tags[client.focus.screen][index] then
			   awful.client.toggletag(tags[client.focus.screen][index])
		   end
	   end)
	)
end
do
	local keynumber = last_tag - 1
	for key = 1, keynumber do
		register_tagkey(key, key)
	end
	register_tagkey(0, last_tag)
end
