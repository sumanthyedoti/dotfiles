## copy URL
xdotool key ctrl+l
xdotool key ctrl+c
xdotool key Escape
url=$(xclip -o)
if [[ $url != *youtube* ]]; then
  notify-send " Not a YT URL" -u critical -t 2000
  exit 1  # Exit with an error code
fi

output_dir="$HOME/ydt"
mkdir -p $output_dir

cd $output_dir

echo $url
notify-send " Downloading..." -r 238946 -t 60000

play_video() {
  notify-send "  Download complete ✓" -t 2000 -r 238946
  notify-send " Playing the video" -t 3000
  downloaded_video_filename=$(yt-dlp --get-filename -o '%(title)s.%(ext)s' $url)\
    || (notify-send " Failed to get filename" -u critical -t 2000 && exit)
  mpv "$output_dir/$downloaded_video_filename"
}

yt-dlp --output '%(title)s.%(ext)s' -f 'bestvideo[height<=?1080]+bestaudio/best' $url && play_video || notify-send " Failed to download" -u critical -t 2000
