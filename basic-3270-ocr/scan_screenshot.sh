#!/usr/bin/env bash
set -euo pipefail

IMAGE="$1"
OPTIONS_STRING="${2:-}"

if [ -n "${WINDIR:-}" ]; then
  WORKDIR="/c/Users/${USERNAME}/Documents/images"
  # on Windows the directory may be absent - We are not using namespaces 
  # [ -d "$WORKDIR" ] || mkdir -p "$WORKDIR" || exit 1
  mkdir -p "$WORKDIR" || {
    1>&2 echo "ERROR: cannot create WORKDIR: $WORKDIR" 
    exit 1
  } 
else
  WORKDIR=$(mktemp -d)
fi

export WORKDIR
mkdir -p "$WORKDIR"

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

[ -f "$WORKDIR/prepared.png" ] || {
  1>&2 echo "ERROR: ImageMagick $WORKDIR/prepared.png was not created"
  exit 1
}

#
# OCR
#

TESSERACT_TAG='5.3.3-alpine'

# there may be more sophisticated OCR command in the future

docker run --rm -v "$WORKDIR:/work:Z" "jitesoft/tesseract-ocr:$TESSERACT_TAG" /work/prepared.png /work/result

[ -s "$WORKDIR/result.txt" ] || {
  1>&2 echo "ERROR: OCR $WORKDIR/result.txt was not created"
  exit 1
}

cat "$WORKDIR/result.txt"
