#!/usr/bin/env bash

set -euo pipefail

# --- CONFIG ---
INPUT="datasets/final/warp_playlist.csv"
OUTDIR="songs/downloaded"

# --- SETUP ---
mkdir -p "$OUTDIR"

echo "Using CSV: $INPUT"
echo "Output dir: $OUTDIR"
echo

# --- DOWNLOAD LOOP ---
python3 - <<EOF
import csv, subprocess, os

input_file = "$INPUT"
outdir = "$OUTDIR"

with open(input_file, newline='', encoding='utf-8') as f:
    reader = csv.DictReader(f)
    for i, row in enumerate(reader, 1):
        track = row['Track Name']
        artist = row['Artist Name(s)']
        query = f"{track} {artist}"

        print(f"[{i}] Downloading: {artist} - {track}")

        subprocess.run([
            "yt-dlp",
            f"ytsearch1:{query} audio",
            "-x",
            "--audio-format", "mp3",
            "--audio-quality", "0",
            "--embed-metadata",
            "--no-overwrites",
            "-o", os.path.join(outdir, "%(artist)s - %(title)s.%(ext)s")
        ])
EOF
