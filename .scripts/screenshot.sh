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
      && magick mogrify -resize 75% $output\
      && pngquant --quality=40-60 --force $output -o $output \
      && notify\
      || exit
    ;;
	"select-good")
    scrot "$output" --select --freeze --line mode=edge\
      && magick mogrify -resize 80% $output\
      && notify\
      || exit
    ;;
	"window")
    scrot "$output" --focused --border\
      && magick mogrify -resize 80% $output\
      && pngquant --quality=80-90 --force $output -o $output \
      && notify\
      || exit
    ;;
  *)
    notify-send "Not a recognized command." -u critical -t 2000
    ;;
esac

