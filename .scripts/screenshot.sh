#! /bin/sh

output_dir="$HOME/Pictures/Screenshots"
mkdir -p $output_dir
timestamp=$(date +'%Y-%m-%d_%H-%M-%S')
output="$output_dir/$timestamp-screenshot.png"

echo $output


notify() {
  notify-send "Screenshot taken." -t 2000
}

case "$1" in
	"select")
    scrot "$output" --select --freeze --line mode=edge\
      && optimizt $output\
      && notify\
      || exit
    ;;
	"window")
    scrot "$output" --focused --border\
      && optimizt $output\
      && notify\
      || exit
    ;;
  *)
    notify-send "Not a recognized command." -u critical -t 2000
    ;;
esac

