local naughty = require("naughty")
-- Copy from example config
if awesome.startup_errors then
    naughty.notify({
        preset = naughty.config.presets.critical,
        title = "Oops, there were errors during startup!",
        text = awesome.startup_errors })
end

-- Handle runtime errors after startup
local in_error = false
awesome.connect_signal("debug::error", function (err)
    -- Make sure we don't go into an endless error loop
    if in_error then return end
    in_error = true

    local stacktrace = debug.traceback()

    naughty.notify({
      title = "An uncaught error happened!",
      text = tostring(err) .. ", " .. tostring(stacktrace),
      preset = naughty.config.presets.critical
    })
    in_error = false
end)

awesome.connect_signal("debug::deprecation", function (hint)
    naughty.notify({
      title = "Deprecation Warning",
      text = tostring(hint),
      preset = naughty.config.presets.warning
    })
end)


-- add restart early
local myutil = require('lib/myutil')
local const = require('rc/const')
ROOT_KEYS = awful.util.table.join(
  ROOT_KEYS,
	awful.key({const.altkey, "Control", "Shift"}, "r", function()
    local check = myutil.rexec("awesome -k 2>&1")
    if string.find(check, 'syntax OK') then
        awesome.restart()
    else
        myutil.notify("Conf Syntax Error!", check)
    end
  end))
root.keys(ROOT_KEYS)
return {}
