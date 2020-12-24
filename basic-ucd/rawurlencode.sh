#!/bin/bash

# based on: https://gist.github.com/moyashi/4063894
# see also https://stackoverflow.com/questions/32273446/encode-url-variable-with-curl

rawurlencode() {

local INPUT_STRING="${1}"
# NOTE debug printout need to be redirected to STDERR
# 1>&2 echo "Processing $INPUT_STRING"
local INPUT_STRING_URLENCODED=$(
awk -v VAR="$INPUT_STRING" -e 'BEGIN{ for (I = 0; I <= 255; I++) { ord[sprintf("%c", I)] = I; } } function escape(DATA, c, len, res) { len = length(DATA) ; res = ""; for (i = 1; i <= len; i++) { c = substr(DATA, i, 1); if(c ~ /[0-9A-Za-z]/){ res = res c; } else { res = res "%" sprintf("%02X", ord[c]); } } return res; } END{ print escape(VAR); }' /dev/null
)
echo $INPUT_STRING_URLENCODED
}

rawurlencode_with_tmpfile() {
local TMP_FILE="/tmp/z$$.txt"
local INPUT_STRING="${1}"
(
awk -v VAR="$INPUT_STRING" -e 'BEGIN{ for (I = 0; I <= 255; I++) { ord[sprintf("%c", I)] = I; } } function escape(DATA, c, len, res) { len = length(DATA) ; res = ""; for (i = 1; i <= len; i++) { c = substr(DATA, i, 1); if(c ~ /[0-9A-Za-z]/){ res = res c; } else { res = res "%" sprintf("%02X", ord[c]); } } return res; } END{ print escape(VAR); }' /dev/null
) > $TMP_FILE
local INPUT_STRING_URLENCODED=$(cat $TMP_FILE)
rm -f $TMP_FILE
echo $INPUT_STRING_URLENCODED
}



DATA=${1:-A-B:C\"/D\\012}
DATA_URLENCODED=$(rawurlencode $DATA)
echo 'Input data:' $DATA
echo 'URL encoded data:' $DATA_URLENCODED
DATA_URLENCODED=$(rawurlencode_with_tmpfile $DATA)
echo 'Input data:' $DATA
echo 'URL encoded data:' $DATA_URLENCODED
exit
