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


local myutil = require("lib/myutil")
local beautiful    = require("beautiful")
local wibox        = require("wibox")
local awful        = require('awful')
local aw_rules     = require('awful.rules')
local pairs        = pairs
local setmetatable = setmetatable
local naughty      = require("naughty")
local table        = table
local clock        = os.clock
local tostring     = tostring
local capi         = {
    awesome        = awesome,
    tag            = tag,
    client         = client,
    keygrabber     = keygrabber,
    mousegrabber   = mousegrabber,
    mouse          = mouse,
    screen         = screen
}

-- disable for now.
-- It seems there is not way to pass err handling function into the delayed_call()

local function debuginfo(message)
    message = message or "No information available"
    nid = naughty.notify({ text = tostring(message), timeout = 10 })
end

local refresh_awesome = function()
  capi.awesome.emit_signal("refresh")
end

local hintbox = {} -- char -> wibox
local hintindex = {} -- char -> client

local clients = {} --Table of clients to be exposed after fitlering
local clientData = {} -- table that holds the positions and sizes of floating clients

local rvl_tag = {}  -- screen -> tag
local rvl_tag_zoom = {} -- screen -> zoomed tag

local revelation = {
    tag_name = "Revelation",
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



-- Executed when user selects a client from expose view.
local function selectfn()
  return function(c)
    revelation.restore()

    refresh_awesome()

    if awful.util.table.hasitem(hintindex, c) then
      -- bring up
      c.minimized = false
      c:jump_to()
    end
  end
end

-- Tags all matching clients with tag t
-- @param rule The rule. Conforms to awful.rules syntax.
-- @param clients A table of clients to check.
-- @param t The tag to put matching clients.
local function match_clients(rule, available_clients, t)
    local match_func = rule.any and awful.rules.match_any or awful.rules.match
    for _, c in pairs(available_clients) do
      if match_func(c, rule) then
        clientData[c] = {}
        clientData[c]["geometry"] = c:geometry()
        for k, v in pairs(revelation.property_to_watch) do
          clientData[c][k] = c[k]
          c[k] = v
        end

        c:toggle_tag(t)
        table.insert(clients, c)
      end
    end
end



function revelation.restore()
    for s in capi.screen do
        -- TODO doens't work with two extra tags
        awful.tag.history.restore(s)
    end

    capi.keygrabber.stop()
    capi.mousegrabber.stop()

    for _, c in pairs(clients) do
      if clientData[c] then
        for k,v in pairs(clientData[c]) do
          if v ~= nil then
            if k== "geometry" then
              c:geometry(v)
            else
              c[k]=v
            end
          end
        end
      end
    end

    for i,j in pairs(hintindex) do
        hintbox[i].visible = false
    end
    for scr in capi.screen do
        rvl_tag_zoom[scr]:delete()
        rvl_tag[scr]:delete()
    end
end

local function hintbox_display_toggle(except_c, show)
  for char, thisclient in pairs(hintindex) do
    if char ~= except_c then
      hintbox[char].visible = show
    end
  end
end

local function hintbox_pos(char)
    local client = hintindex[char]
    local geom = client:geometry()
    hintbox[char].x = math.floor(geom.x + geom.width/2 - revelation.hintsize/2)
    hintbox[char].y = math.floor(geom.y + geom.height/2 - revelation.hintsize/2)
end

local function hide_all_boxes()
  for _, b in pairs(hintbox) do
    b.visible = false
  end
end

local function zoom_client(char)
  local c = hintindex[char]
  rvl_tag_zoom[c.screen]:view_only()
  c:toggle_tag(rvl_tag_zoom[c.screen])
  -- refresh the clients, since it is moved
  refresh_awesome()
  hintbox_pos(char)
  hintbox_display_toggle(char, false)
  return true
end

local function unzoom_client(char)
  local c = hintindex[char]
  if c then -- client might have closed during the zoom
    awful.tag.history.restore(c.screen)
    c:toggle_tag(rvl_tag_zoom[c.screen])
  end
  hintbox_display_toggle(char, true)
  if c then
    refresh_awesome()
    hintbox_pos(char)
  end
end

local function expose_callback(clientlist)
    -- draw wiboxes
    hintindex = {}
    for i, cur_client in pairs(clientlist) do
      local char = revelation.charorder:sub(i,i)
      if char and char ~= '' then
        hintindex[char] = cur_client
        local cur_wibox = hintbox[char]
        hintbox_pos(char)
        cur_wibox.visible = true
        cur_wibox.screen = cur_client.screen
      end
    end

    local key_char_zoomed = nil

    capi.keygrabber.run(function (mod, key, event)
        -- avoid handling shift and release
        if event == "release" then return true end
        if key == "Shift_L" or key == "Shift_R" then return true end

        -- handle upper case: either zoom or unzoom
        if awful.util.table.hasitem(mod, "Shift") then
            key_char = string.lower(key)
            local c = hintindex[key_char]

            -- not zoom -> zoom
            if not key_char_zoomed and c ~= nil then
              zoom_client(key_char)
              key_char_zoomed = key_char
              return true
              -- zoom -> not zoom.
            elseif key_char_zoomed ~= nil then
              unzoom_client(key_char_zoomed)
              key_char_zoomed = nil
              return true
            end
        end

        -- handle selection
        if hintindex[key] then
          selectfn()(hintindex[key])
          hide_all_boxes()
          return false
        end

        -- any all other keys are cancel
        if key_char_zoomed ~= nil then
          unzoom_client(key_char_zoomed)
          key_char_zoomed = nil
          return true
        else
          hide_all_boxes()
          revelation.restore()
          return false
        end
    end)
    -- end of keyboard

    local pressing_middle = false
    local lastClient = nil
    capi.mousegrabber.run(function(mouse)
        -- state machine of "first press event"
        local on_press_middle = (mouse.buttons[2] == true and pressing_middle == false)
        local on_release_middle = (mouse.buttons[2] == false and pressing_middle == true)
        if on_press_middle then pressing_middle = true end
        if on_release_middle then -- allow release happen anywhere
          pressing_middle = false
          for key, _ in pairs(hintindex) do
              hintbox_pos(key)
          end
          return true
        end

        -- get the chosen client
        local c = capi.mouse.current_client
        if c then
          lastClient = c  -- continuous caching state
        else
          local current_wibox = capi.mouse.current_wibox
          if current_wibox then
            local check_this_wibox = awful.util.table.hasitem(hintbox, current_wibox)
            if check_this_wibox then c = lastClient end
          end
        end
        local key_char = awful.util.table.hasitem(hintindex, c)

        -- if (mouse.buttons[1] or mouse.buttons[2]) and (not c) then return false end  -- click on nothing
        if mouse.buttons[1] == true then
          selectfn()(c)
          hide_all_boxes()
          return false
        elseif on_press_middle then
          c:kill()
          hintbox[key_char].visible = false
          hintindex[key_char] = nil
          local pos = awful.util.table.hasitem(clients, c)
          table.remove(clients, pos)

          -- unzoom
          if key_char_zoomed ~= nil then
            unzoom_client(key_char_zoomed)
            key_char_zoomed = nil
          end
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

      hintbox[char] = wibox({
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

    clients = {}
    clientData = {}
    local base_tname = revelation.tag_name

    for scr in capi.screen do
        local tags = awful.tag.new(
            {base_tname, base_tname .. '_zoom'},
            scr, awful.layout.suit.fair)
        rvl_tag[scr] = tags[1]
        rvl_tag_zoom[scr] = tags[2]

        if curr_tag_only then
            match_clients(rule, scr.clients, rvl_tag[scr])
        else
            match_clients(rule, scr.all_clients, rvl_tag[scr])
        end

        rvl_tag[scr]:view_only()
    end

    refresh_awesome()

    local status, err = pcall(expose_callback, clients)

    if not status then
      debuginfo('Oops!, something is wrong in revelation.expose_callback!')
      if err.msg then debuginfo(err.msg) end
      if err.code then debuginfo('error code is '.. tostring(err.code)) end
      revelation.restore()
    end
end

setmetatable(revelation, { __call = function(_, ...) return revelation.expose(...) end })

return revelation
