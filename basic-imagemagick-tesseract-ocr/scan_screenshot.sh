#!/usr/bin/env bash
set -euo pipefail

IMAGE="$1"
OPTIONS_STRING="$2"
WORKDIR=$(mktemp -d)
trap 'rm -rf "$WORKDIR"' EXIT

chmod 777 "$WORKDIR"

cp "$IMAGE" "$WORKDIR/input.png"

#
# Image preprocessing candidate.
#
# The current transformation is deliberately conservative.
# A future ML/ranking loop can substitute or enumerate
# alternative ImageMagick transformations here.
#
if [ ! -n "$OPTIONS_STRING" ] ; then 
  docker run --rm -v "$WORKDIR:/work:Z" minidocks/imagemagick magick /work/input.png $OPTIONS_STRING /work/prepared.png
else 
  docker run --rm -v "$WORKDIR:/work:Z" minidocks/imagemagick magick /work/input.png  -colorspace Gray -density 300 /work/prepared.png
fi
#
# OCR
#
docker run --rm -v "$WORKDIR:/work:Z" jitesoft/tesseract-ocr /work/prepared.png /work/result

cat "$WORKDIR/result.txt"
