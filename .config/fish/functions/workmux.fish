function workmux
    if test "$argv[1]" = add
        for var in DISPLAY WAYLAND_DISPLAY XAUTHORITY DBUS_SESSION_BUS_ADDRESS XDG_RUNTIME_DIR
            set val (printenv $var 2>/dev/null)
            if test -n "$val"
                command tmux set-environment -g $var $val
            end
        end
    end
    command workmux $argv
end
