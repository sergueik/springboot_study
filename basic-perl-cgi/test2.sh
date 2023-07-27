#!/bin/sh
# origin: https://stackoverflow.com/questions/50979911/how-to-test-perl-cgi-script-for-file-upload-from-the-command-line
SCRIPT=${1:-cgi-bin/upload.cgi}
FILENAME=${2:-./data.txt}
export REQUEST_METHOD=POST
export CONTENT_TYPE='multipart/form-data; boundary=xYzZY'
PAYLOAD=$( perl -MHTTP::Request::Common=POST -e "print POST( undef, Content_Type => 'form-data', Content => [ new => 1, type => 'send', data => ['$FILENAME'] ])->content ;")
echo 'Payload:'
echo $PAYLOAD

echo $PAYLOAD| perl $SCRIPT 

