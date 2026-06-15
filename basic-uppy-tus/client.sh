#!/bin/bash

FILE=${1:-"test.txt"}
CHUNK_SIZE=${2:-"256"}
URL=${3:-"http://localhost:8080/api/upload"}

1>&2 echo 'Uploading: '

RESP=$(curl -si -X POST "$URL" -H 'Tus-Resumable: 1.0.0' -H 'Upload-Defer-Length: 1')

1>&2 echo "RESP=$RESP"
LOCATION=$(echo "$RESP" |grep 'Location:' |awk '{print $2}'|tr -d '\r')

1>&2 echo "Create upload LOCATION: $LOCATION"

SIZE=$(wc -c < "$FILE")
1>&2 echo "Get file size: $SIZE"
OFFSET=0
BASE_URL="http://localhost:8080"
PATCH_URL="${BASE_URL}${LOCATION}"
1>&2 echo "Uploading to : $PATCH_URL"
while [ $OFFSET -lt $SIZE ]
do
  END=$((OFFSET + CHUNK_SIZE))
  if [ $END -gt $SIZE ]; then
    END=$SIZE
  fi

  CNT=$((END - OFFSET))

  # NOTE: do not store upload data in a shell variable
  # it is prone to stripping the trailing newline

  TMPFILE=$(mktemp)
  dd if="$FILE" bs=1 skip=$OFFSET count=$CNT of="$TMPFILE" 2>/dev/null

  1>&2 echo "Uploading: offset=$OFFSET len=$CNT"
  1>&2 echo "curl -si -X PATCH \"$PATCH_URL\" -H 'Tus-Resumable: 1.0.0' -H \"Upload-Offset: $OFFSET\" -H 'Content-Type: application/offset+octet-stream' --data-binary \"@$TMPFILE\""

  curl -si -X PATCH "$PATCH_URL" -H 'Tus-Resumable: 1.0.0' -H "Upload-Offset: $OFFSET" -H 'Content-Type: application/offset+octet-stream' --data-binary "@$TMPFILE"

  OFFSET=$END
done
ID=$(echo $LOCATION | cut -f 4 -d '/')
sha256sum $FILE
sha256sum "$TEMP/tus/uploads/$ID/data"
# NOTE: the e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855 is the hash of an empty file

