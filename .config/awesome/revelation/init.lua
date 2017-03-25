-- revelation.lua
--
-- Library that implements Expose like behavior.
--
-- @author Perry Hargrave resixian@gmail.com
-- @author Espen Wiborg espenhw@grumblesmurf.org
-- @author Julien Danjou julien@danjou.info
-- @auther Quan Guo guotsuan@gmail.com
--
-- @copyright 2008 Espen Wiborg, Julien Danjou
-- @copyright 2015 Quan Guo
--


local wibox        = require("wibox")
local awful        = require('awful')
local capi         = {
  awesome        = awesome,
  tag            = tag,
  client         = client,
  keygrabber     = keygrabber,
  mousegrabber   = mousegrabber,
  mouse          = mouse,
  screen         = screen
}

local refresh_awesome = function()
  capi.awesome.emit_signal("refresh")
end

local hint_box = {} -- char -> wibox
local hint_client = {} -- char -> client

local client_state = {} -- table that holds the positions and sizes of floating clients

local rvl_tag = {}  -- screen -> tag
local rvl_tag_zoom = {} -- screen -> zoomed tag

local revelation = {
  tag_name = "Windows",
  charorder = "jkluiopyhnmfdsatgvcewqzx1234567890",

  property_to_watch={
    minimized            = false,
    fullscreen           = false,
    maximized_horizontal = false,
    maximized_vertical   = false,
    sticky               = false,
    ontop                = false,
    above                = false,
    below                = false,
    floating             = false,
  },
  curr_tag_only = false,
  font = "monospace 30",
  fg = "#42f4cb",
  bg = "#000000",
  border_color=beautiful.border_focus or "#DCDCCC",
  border_width=beautiful.border_width or 2,
  hintsize = (type(beautiful.xresources) == 'table' and beautiful.xresources.apply_dpi(60) or 60)
}

local function set_box_vis(vis, except_c)
  for char, box in pairs(hint_box) do
    if char ~= except_c and hint_client[char] ~= nil then
      box.visible = vis
    end
  end
end

-- Tags all matching clients with tag t
-- @param rule The rule. Conforms to awful.rules syntax.
-- @param clients A table of clients to check.
-- @param t The tag to put matching clients.
local function match_clients(rule, available_clients, t)
  local clients = {}
  local match_func = rule.any and awful.rules.match_any or awful.rules.match
  for _, c in pairs(available_clients) do
    if match_func(c, rule) then
      client_state[c] = {}
      client_state[c]["geometry"] = c:geometry()
      for k, v in pairs(revelation.property_to_watch) do
        client_state[c][k] = c[k]
        c[k] = v
      end

      c:toggle_tag(t)
      table.insert(clients, c)
    end
  end
  return clients
end

local function expose_restore()
  -- restore tags
  for s in capi.screen do
    awful.tag.history.restore(s)
  end

  capi.keygrabber.stop()
  capi.mousegrabber.stop()

  -- clear boxes
  set_box_vis(false, nil)

  -- resume locations
  for c, cdata in pairs(client_state) do
    if c ~= nil then  -- may have closed
      for k, v in pairs(cdata) do
        if k == "geometry" then c:geometry(v)
        else c[k]=v end
      end
    end
  end

  -- delete tags
  for scr in capi.screen do
    rvl_tag_zoom[scr]:delete()
    rvl_tag[scr]:delete()
  end
end

local function update_box_pos(char)
  local client = hint_client[char]
  local geom = client:geometry()
  hint_box[char].x = math.floor(geom.x + geom.width/2 - revelation.hintsize/2)
  hint_box[char].y = math.floor(geom.y + geom.height/2 - revelation.hintsize/2)
end

local function zoom_client(char)
  local c = hint_client[char]
  if c then
    rvl_tag_zoom[c.screen]:view_only()
    c:toggle_tag(rvl_tag_zoom[c.screen])
    -- refresh the clients, since it is moved
    refresh_awesome()
    update_box_pos(char)
  end
  set_box_vis(false, char)
  return true
end

local function unzoom_client(char)
  local c = hint_client[char]
  if c then -- client might have closed during the zoom
    awful.tag.history.restore(c.screen)
    c:toggle_tag(rvl_tag_zoom[c.screen])
  end
  if c then
    refresh_awesome()
    update_box_pos(char)
  end
  set_box_vis(true, char)
end

local function remove_client(char, clt)
  client_state[clt] = nil
  clt:kill()
  hint_box[char].visible = false
  hint_client[char] = nil
end

local function select_client(c)
  expose_restore()

  if awful.util.table.hasitem(hint_client, c) then
    c.minimized = false
    c:jump_to()
  end
end

local function expose_interactive()
  local key_char_zoomed = nil

  capi.keygrabber.run(function (mod, key, event)
    -- avoid handling shift and release
    if event == "release" then return true end
    if key == "Shift_L" or key == "Shift_R" then return true end

    -- handle upper case: either zoom or unzoom
    if awful.util.table.hasitem(mod, "Shift") then
      local real_key = string.lower(key)

      -- zoom -> not zoom.
      if key_char_zoomed ~= nil then
        unzoom_client(key_char_zoomed)
        key_char_zoomed = nil
        -- not zoom -> zoom
      else
        zoom_client(real_key)
        key_char_zoomed = real_key
      end
      return true
    end

    -- handle selection
    if hint_client[key] then
      select_client(hint_client[key])
      return false
    end

    -- any all other keys are cancel
    if key_char_zoomed ~= nil then
      unzoom_client(key_char_zoomed)
      key_char_zoomed = nil
      return true
    else
      expose_restore()
      return false
    end
  end)
  -- end of keyboard

  local pressing_middle = false
  capi.mousegrabber.run(function(mouse)
    -- state machine of "first press event"
    local on_press_middle = (mouse.buttons[2] == true and pressing_middle == false)
    local on_release_middle = (mouse.buttons[2] == false and pressing_middle == true)
    if on_press_middle then pressing_middle = true end
    if on_release_middle then -- allow release happen anywhere
      pressing_middle = false
      for key, _ in pairs(hint_client) do
        update_box_pos(key)
      end
      set_box_vis(true, nil)
      return true
    end

    -- get the chosen client
    local c = capi.mouse.current_client
    if not c then
      local current_wibox = capi.mouse.current_wibox
      if current_wibox then
        local char = awful.util.table.hasitem(hint_box, current_wibox)
        if char then c = hint_client[char] end
      end
    end
    local key_char = awful.util.table.hasitem(hint_client, c)

    -- right click or click on nothing
    if mouse.buttons[3] or (
        (mouse.buttons[1] or mouse.buttons[2]) and (not c)) then
      expose_restore()
      return false
    end

    if mouse.buttons[1] == true then
      select_client(c)
      return false
    elseif on_press_middle then
      remove_client(key_char, c)
      -- unzoom
      if key_char_zoomed ~= nil then
        unzoom_client(key_char_zoomed)
        key_char_zoomed = nil
      end
      set_box_vis(false, nil)
      return true
    end
    return true
    --Strange but on my machine only fleur worked as a string.
    --stole it from https://github.com/Elv13/awesome-configs/blob/master/widgets/layout/desktopLayout.lua#L175
  end,"fleur")
end

-- Create the wiboxes, but don't show them
function revelation.init(args)
  args = args or {}

  revelation.tag_name = args.tag_name or revelation.tag_name
  revelation.charorder = args.charorder or revelation.charorder

  for i = 1, #revelation.charorder do
    local char = revelation.charorder:sub(i, i)
    local letterbox = wibox.widget.textbox()
    letterbox:set_markup(char:upper())
    letterbox:set_font(revelation.font)
    letterbox:set_align("center")

    hint_box[char] = wibox({
      fg = revelation.fg,
      bg = revelation.bg,
      border_color = revelation.border_color,
      border_width = revelation.border_width,
      width = revelation.hintsize,
      height = revelation.hintsize,
      ontop = true,
      widget = letterbox,
      visible = false,
    })
  end
end


-- @param rule A table with key and value to match. [{class=""}]
function revelation.expose(args)
  args = args or {}
  local rule = args.rule or {}
  local curr_tag_only = args.curr_tag_only or revelation.curr_tag_only

  local clients = {}
  client_state = {}
  local base_tname = revelation.tag_name

  -- build new tags and switch
  for scr in capi.screen do
    local tags = awful.tag.new(
        {base_tname, base_tname .. 'Zoom'},
        scr, awful.layout.suit.fair)
    rvl_tag[scr] = tags[1]
    rvl_tag_zoom[scr] = tags[2]

    local candidate_clients = curr_tag_only and scr.clients or scr.all_clients

    for _, c in ipairs(
        match_clients(rule, candidate_clients, rvl_tag[scr])) do
      table.insert(clients, c)
    end

    rvl_tag[scr]:view_only()
  end

  refresh_awesome()

  -- assign and show wiboxes
  hint_client = {}
  for i, cur_client in pairs(clients) do
    local char = revelation.charorder:sub(i,i)
    if char and char ~= '' then
      hint_client[char] = cur_client
      local cur_wibox = hint_box[char]
      update_box_pos(char)
      cur_wibox.visible = true
      cur_wibox.screen = cur_client.screen
    end
  end

  expose_interactive()
end

setmetatable(revelation, { __call = function(_, ...) return revelation.expose(...) end })

return revelation
