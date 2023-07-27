#!/bin/bash
# origin: https://stackoverflow.com/questions/50979911/how-to-test-perl-cgi-script-for-file-upload-from-the-command-line
SCRIPT=${1:-cgi-bin/upload.cgi}
FILENAME=${2:-./data.txt}
REQUEST_METHOD=POST \
CONTENT_TYPE='multipart/form-data; boundary=xYzZY' \
perl $SCRIPT < <(
    perl -MHTTP::Request::Common=POST -e "
print POST(
    undef,
    Content_Type => 'form-data',
    Content      => [ new => 1, type => 'send', data => ['$FILENAME'] ]
  )->content ;
    "
)

