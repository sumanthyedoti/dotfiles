#!/bin/sh

# The xinitrc file is read by the startx command, which is often used when you start an X session manually,
# without a display manager. It is responsible for launching the X server, window manager,
# and any other applications you want to run in your X session.

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

echo "XINITRC"
xrandr --output HDMI1 --mode 3840x2160 --primary
exec xmonad

# Wallpaper
nitrogen --restore &

if [ ! -d /dev/d2 ]; then
  sudo mkdir /dev/d2
fi

# Compositior
picom -f &
