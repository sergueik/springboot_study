#!/bin/bash

# NOTE: Standard getopt only natively supports single-character letters and numbers 
# For longer word-based options (e.g., --help or --output), you can use getopt_long in C/C++ or built-in utilities like the enhanced GNU getopt in Bash
OPTS=`getopt -o dvf:b:n:t:p:u:y --long debug,verbose,foo,bar,filename,type,codepage,url,dry-run -n 'parse-options' -- "$@"`
if [ $? != 0 ] ; then echo "Failed parsing options." >&2 ; exit 1 ; fi

while true; do
  case "$1" in
    -h | --help )  HELP=true; shift ;;
    -d | --debug ) DEBUG=true; shift ;;
    -v | --verbose ) VERBOSE=true; shift ;;
    -f | --foo ) FOO="$2"; shift; shift ;;
    -b | --bar ) BAR="$2"; shift; shift ;;
    -n | --name ) FILENAME="$2"; shift; shift ;;
    -t | --type ) CONTENT_TYPE="$2"; shift; shift ;;
    -p | --codepage ) CODEPAGE="$2"; shift; shift ;;
    -u | --url ) URL="$2"; shift; shift ;;
    -z | --dry-run )   DRY_RUN=true; shift ;;
    -- ) shift; break ;;
    * ) break ;;
  esac
done

if [[ "${HELP}" = "true" ]]; then
cat <<EOF
Usage:
  $0 [-d|--debug] [-h|--help] [-v|--verbose] [-s|--size] SIZE [-n|--filena,ename] FILENAME [-p|--codepage] CODEPAGE [-u|--url] URL ...
EOF
exit 0
fi
if [[ "${VERBOSE}" = "true" ]]; then
  if $DRY_RUN ; then
    echo 'demonstrate commands'
  else
    echo 'run commands'
  fi
  echo VERBOSE=$VERBOSE
  echo DEBUG=$DEBUG
  echo CODEPAGE=$CODEPAGE
  echo FILENAME=$FILENAME
  echo FOO=$FOO
  echo BAR=$BAR
  echo BAR=$BAR
  echo CONTENT_TYPE=$CONTENT_TYPE
  echo URL=$URL
fi


# TODO: get rid of shell variable
# base64 -w0 < "$FILENAME" > "$TMPBASE64"
if [ "$CODEPAGE" != "UTF-8" ]; then 
  BASE64=$(
    iconv \
      -f $CODEPAGE \
      -t UTF-8 \
      $FILENAME |
    base64 -w0
  );
else
  BASE64=$(base64 -w0 < $FILENAME)
fi
TMPFILE=/tmp/data.$$.json

jq -n \
  --arg foo "$FOO" \
  --arg bar "$BAR" \
  --arg filename "$FILENAME" \
  --arg contentType "$CONTENT_TYPE" \
  --arg contentBase64 "$BASE64" \
'{
  foo: $foo,
  bar: $bar,
  filename: $filename,
  contentType: $contentType,
  contentBase64: $contentBase64
}' > $TMPFILE

curl \
   -H 'Content-Type: application/json' \
   -d@$TMPFILE \
  $URL
