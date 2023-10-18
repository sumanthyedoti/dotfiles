#! /bin/sh

mkdir -p /home/sumanthyedoti/Pictures/Screenshots/
output=~/Pictures/Screenshots/%Y-%m-%d-%T-screenshot.png

notify() {
  notify-send "Screenshot taken." -t 2000
}

case "$1" in
	"select")
    scrot "$output" --select --freeze --line mode=edge && notify || exit
    ;;
	"window")
    scrot "$output" --focused --border && notify || exit
    ;;
  *)
    notify-send "Not a recognized command." -u critical -t 2000
    ;;
esac

