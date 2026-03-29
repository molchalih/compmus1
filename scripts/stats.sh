#!/bin/bash

DIR="${1:-.}"

total=0
count=0
min=999999
max=0

echo "Track durations:"
echo "----------------"

for f in "$DIR"/*.{mp3,wav,flac,aiff}; do
  [ -e "$f" ] || continue

  duration=$(ffprobe -v error -show_entries format=duration \
    -of default=noprint_wrappers=1:nokey=1 "$f")

  duration=$(printf "%.2f" "$duration")

  echo "$(basename "$f"): $duration sec"

  total=$(echo "$total + $duration" | bc)
  count=$((count + 1))

  # min
  is_less=$(echo "$duration < $min" | bc)
  if [ "$is_less" -eq 1 ]; then
    min=$duration
  fi

  # max
  is_more=$(echo "$duration > $max" | bc)
  if [ "$is_more" -eq 1 ]; then
    max=$duration
  fi
done

echo "----------------"

if [ "$count" -gt 0 ]; then
  avg=$(echo "scale=2; $total / $count" | bc)

  echo "Tracks: $count"
  echo "Min: $min sec"
  echo "Max: $max sec"
  echo "Avg: $avg sec"
else
  echo "No audio files found."
fi
