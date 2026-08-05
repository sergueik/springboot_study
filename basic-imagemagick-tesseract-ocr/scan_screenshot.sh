#!/usr/bin/env bash
set -euo pipefail

IMAGE="$1"

WORKDIR=$(mktemp -d)

chmod 777 "$WORKDIR"

cp "$IMAGE" "$WORKDIR/input.png"

# optional preprocessing
docker run --rm \
  -v "$WORKDIR:/work:Z" \
  minidocks/imagemagick \
  magick /work/input.png \
          -colorspace Gray \
          -density 300 \
          /work/prepared.png
#ls -l $WORKDIR

# OCR
docker run --rm \
  -v "$WORKDIR:/work:Z" \
  jitesoft/tesseract-ocr \
  /work/prepared.png \
  /work/result

cat "$WORKDIR/result.txt"
rm -fr $WORKDIR
