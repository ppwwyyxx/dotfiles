local function movemouse_rel(x, y)
    mouse.coords{x = mouse.coords().x + x, y = mouse.coords().y + y}
end

local function movemouse_perc(px, py)
    local g = screen[mouse.screen].workarea
    mouse.coords{x = g.x + g.width * px, y = g.y + g.height * py}
end

function mouse_control()
    local step, bigstep = 20, 80
    keygrabber.run(function(mod, key, event)
                   if event == 'release' and key then return end
                   if key == 'k' then movemouse_rel(0, -step)
                   elseif key == 'j' then movemouse_rel(0, step)
                   elseif key == 'h' then movemouse_rel(-step, 0)
                   elseif key == 'l' then movemouse_rel(step, 0)

                   elseif key == 'K' then movemouse_rel(0, -bigstep)
                   elseif key == 'J' then movemouse_rel(0, bigstep)
                   elseif key == 'H' then movemouse_rel(-bigstep, 0)
                   elseif key == 'L' then movemouse_rel(bigstep, 0)

                   elseif key == 't' then movemouse_perc(0.3, 0.2)
                   elseif key == 'u' then movemouse_perc(0.7, 0.2)
                   elseif key == 'v' then movemouse_perc(0.3, 0.8)
                   elseif key == 'n' then movemouse_perc(0.7, 0.8)
                   elseif key == 'g' then movemouse_perc(0.5, 0.5)

                   elseif key == 'a' then movemouse_perc(0.15, 0.5)
                   elseif key == 'q' then movemouse_perc(0.15, 0.2)
                   elseif key == 'z' then movemouse_perc(0.15, 0.8)
                   elseif key == ';' then movemouse_perc(0.85, 0.5)
                   elseif key == 'p' then movemouse_perc(0.85, 0.2)
                   elseif key == '.' then movemouse_perc(0.85, 0.8)

                   elseif key == ' ' then       -- TODO how to click properly??
                       exec("xdotool click --clearmodifiers 1")
                   elseif key == 'r' then
                       exec("xdotool click --clearmodifiers 3")
                       keygrabber.stop()
                   elseif key == 'Return' then
                       exec("xdotool click --clearmodifiers 1")
                       keygrabber.stop()
                   elseif key == 'Shift_L' or key == 'Shift_R' then return
                   else
                       keygrabber.stop()
                   end
               end)

end
