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
local tagData = {}

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
-- @param restore Function to reset the current tags view.
local function selectfn(_, t, zt)
    return function(c)
        revelation.restore(t, zt)
        -- Focus and raise
        --
        if type(delayed_call) == 'function' then
            capi.awesome.emit_signal("refresh")
        end

        if awful.util.table.hasitem(hintindex, c) then
            if c.minimized then
                c.minimized = false
            end

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



function revelation.restore(t, zt)
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
        zt[scr]:delete()
        t[scr]:delete()
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

local function expose_callback(t, zt, clientlist)
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

    local zoomed = false
    local zoomed_client = nil
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
            if not zoomed and c ~= nil then
                zt[c.screen]:view_only()
                c:toggle_tag(zt[c.screen])
                zoomed_client = c
                key_char_zoomed = key_char
                zoomed = true
                -- refresh the clients, since it is moved
                refresh_awesome()
                hintbox_pos(key_char)
                hintbox_display_toggle(key_char, false)
            -- zoom -> not zoom.
            elseif zoomed_client ~= nil then
                awful.tag.history.restore(zoomed_client.screen)
                zoomed_client:toggle_tag(zt[zoomed_client.screen])
                hintbox_display_toggle(key_char_zoomed, true)
                refresh_awesome()
                hintbox_pos(key_char_zoomed)

                zoomed_client = nil
                zoomed = false
                key_char_zoomed = nil
            end
            return true
        end

        -- handle selection
        if hintindex[key] then
          selectfn(restore, t, zt)(hintindex[key])
          hide_all_boxes()
          return false
        end

        -- any all other keys are cancel
        if zoomed_client ~= nil then
            awful.tag.history.restore(zoomed_client.screen)
            zoomed_client:toggle_tag(zt[zoomed_client.screen])
            hintbox_display_toggle(string.lower(key),  true)
            refresh_awesome()
            hintbox_pos(key_char_zoomed)

            zoomed_client = nil
            zoomed = false
            return true
        else
          hide_all_boxes()
          revelation.restore(t, zt)
          return false
        end
    end)
    -- end of keyboard

    local pressedMiddle = false
    local pressedRight = false
    local lastClient = nil
    capi.mousegrabber.run(function(mouse)
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

        -- c can be nil
        local key_char = awful.util.table.hasitem(hintindex, c)

        if mouse.buttons[1] == true then
          if c ~= nil then
            selectfn(restore, t, zt)(c)
            hide_all_boxes()
            return false
          else
            return true
          end
        elseif mouse.buttons[2] == true and pressedMiddle == false and c ~= nil then
            pressedMiddle = true

            c:kill()
            hintbox[key_char].visible = false
            hintindex[key_char] = nil
            local pos = awful.util.table.hasitem(clients, c)
            table.remove(clients, pos)

            -- unzoom
            if zoomed == true and zoomed_client ~=nil then
                -- zoomed_client might have been killed
                awful.tag.history.restore(zoomed_client.screen)
                zoomed_client:toggle_tag(zt[zoomed_client.screen])
                hintbox_display_toggle(key_char_zoomed, true)
                zoomed_client = nil
                zoomed = false
                key_char_zoomed = nil
            end
            return true
        elseif mouse.buttons[2] == false and pressedMiddle == true then
            pressedMiddle = false
            for key, _ in pairs(hintindex) do
                hintbox_pos(key)
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

    local t = {}  -- screen -> tag
    local zt = {} -- -> zoomed tag

    clients = {}
    clientData = {}
    local base_tname = revelation.tag_name

    for scr in capi.screen do
        local tags = awful.tag.new(
            {base_tname, base_tname .. '_zoom'},
            scr, awful.layout.suit.fair)
        t[scr] = tags[1]
        zt[scr] = tags[2]

        if curr_tag_only then
            match_clients(rule, scr.clients, t[scr])
        else
            match_clients(rule, scr.all_clients, t[scr])
        end

        t[scr]:view_only()
    end

    refresh_awesome()

    local status, err = pcall(expose_callback, t, zt, clients)

    if not status then
      debuginfo('Oops!, something is wrong in revelation.expose_callback!')
      if err.msg then debuginfo(err.msg) end
      if err.code then debuginfo('error code is '.. tostring(err.code)) end
      revelation.restore(t, zt)
    end
end

setmetatable(revelation, { __call = function(_, ...) return revelation.expose(...) end })

return revelation
