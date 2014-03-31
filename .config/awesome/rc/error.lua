
-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        stacktrace = debug.traceback()

        notify("An uncaught error happened!",
               tostring(err) .. ", " .. tostring(stacktrace),
               'critical')
        in_error = false
    end)
end
