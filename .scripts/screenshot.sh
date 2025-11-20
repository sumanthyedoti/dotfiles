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
  scrot "$output" --select --freeze --line style=dash && magick mogrify -resize 75% $output && pngquant --quality=40-60 --force $output -o $output &&
    notify || exit
  ;;
"select-good")
  scrot "$output" --select --freeze --line style=dash && magick mogrify -resize 85% $output && notify || exit
  ;;
"window")
  scrot "$output" --focused --border && magick mogrify -resize 75% $output && pngquant --quality=75-85 --force $output -o $output &&
    notify || exit
  ;;
"window-good")
  scrot "$output" --focused --border $output && pngquant --quality=75-85 --force $output -o $output && notify || exit
  ;;
"clipboard")
  scrot --select --freeze --line style=dash -o /tmp/screenshot.png &&
    magick mogrify -resize 75% /tmp/screenshot.png &&
    pngquant --quality=40-60 --force /tmp/screenshot.png -o /tmp/screenshot.png &&
    xclip -selection clipboard -t image/png -i /tmp/screenshot.png &&
    notify || exit
  ;;
*)
  notify-send "Not a recognized command." -u critical -t 2000
  ;;
esac
