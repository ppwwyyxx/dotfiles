local naughty = require("naughty")
local myutil = require("lib/myutil")
-- Copy from example config
if awesome.startup_errors then
    naughty.notify({
        preset = naughty.config.presets.critical,
        title = "Oops, there were errors during startup!",
        text = awesome.startup_errors })
end
-- Handle runtime errors after startup
do
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
end
