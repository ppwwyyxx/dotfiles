
local myutil = require("lib/myutil")

local _dict_notify
local _old_word

return function()
  local word = myutil.trim(selection())
  if _dict_notify ~= nil then
    naughty.destroy(_dict_notify)
    _dict_notify = nil
    if _old_word == word then return end
  end
  _old_word = word

  awful.spawn.easy_async(
    "sdcv -n --utf8-output '"..word.."'",
    function(stdout, stderr, exitreason, exitcode)
      _dict_notify = naughty.notify({text = stdout, timeout = 5, width = 1020})
    end)
end
