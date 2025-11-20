#!/bin/sh

SS_dir=/home/sumanthyedoti/Pictures/Screenshots/
mkdir -p /home/sumanthyedoti/Pictures/Screenshots/
output=~/Pictures/Screenshots/%Y-%m-%d-%T-screenshot.png

last_SS() {
  cd $SS_dir
  ls -t | head -n 1
}

scrot "$output" --select --freeze --line style=dash
if [ $? -ne 0 ]; then
  notify-send " Failed to take screen shot" -u critical -t 2000
  exit
fi

SS_file="$SS_dir$(last_SS)"
tesseract $SS_file - -l eng | xclip -selection clipboard
if [ $? -eq 0 ]; then
  notify-send "✓ Succefully extracted text form the screenshot" -u low -t 2000
else
  notify-send " Failed to extract the test" -u critical -t 2000
fi
rm $SS_file
