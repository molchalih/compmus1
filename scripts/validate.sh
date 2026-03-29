#!/usr/bin/env bash

BASE="datasets/final"

CHROMA="$BASE/chroma"
TIMBRE="$BASE/timbre"
NOVELTY="$BASE/tempo_novelty"
ACT="$BASE/tempo_act"
DFT="$BASE/tempo_dft"

echo "Checking dataset consistency..."
echo

missing=0

for file in "$CHROMA"/*.csv; do
  name=$(basename "$file")

  echo "Checking: $name"

  for dir in "$TIMBRE" "$NOVELTY" "$ACT" "$DFT"; do
    if [ ! -f "$dir/$name" ]; then
      echo "  ❌ Missing in $(basename "$dir"): $name"
      missing=1
    fi
  done
done

echo

if [ $missing -eq 0 ]; then
  echo "✅ All files are consistent."
else
  echo "⚠️ Some files are missing."
fi
