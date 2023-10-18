#!/bin/sh

# Increment, decrement, or mute the volume and send a notification
# of the current volume level.
id=9993
change_volume() {
	volume=$(pamixer --get-volume)
  icon=""
  if [ "$1" == "down" ]; then
    icon=""
  fi
	dunstify -a "changevolume" -u low -r $id -h int:value:"$volume" "${icon} Volume: ${volume}%" -t 1000
}

case $1 in
  up)
    # Set the volume on (if it was muted)
	  volume=$(pamixer --get-volume)
    if [ "$volume" -ge 100 ]; then
      change_volume "$1"
      return 0
    fi

    pamixer -u
    pamixer -i 5 --allow-boost
    change_volume "$1"
    ;;
  down)
    pamixer -u
    pamixer -d 5 --allow-boost
    change_volume "$1"
    ;;
  mute)
    pamixer -t # mute
    if eval "$(pamixer --get-mute)"; then # toggle mute
      dunstify -a "changevolume" -t 1000 -r $id -u low " Muted"
    else
      change_volume up
    fi
    ;;
  *)
    dunstify -a "changevolume" -u critical -t 2000 -r $id "Not a recognized command."
    ;;
esac
