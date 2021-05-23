#!/bin/bash

NAME=${1:-basic-perl-cgi-container}
DOCUMENT_ROOT='/var/www/localhost/htdocs'
for D in css js ; do docker exec $NAME mkdir $DOCUMENT_ROOT/$D; done
for D in JSON YAML ; do docker exec $NAME mkdir /var/www/localhost/cgi-bin/$D; done
docker cp html/inventory.html $NAME:$DOCUMENT_ROOT
for F in $(ls -1 html/css) ; do docker cp html/css/$F $NAME:$DOCUMENT_ROOT/css; done
for F in $(ls -1 html/js) ; do docker cp html/js/$F $NAME:$DOCUMENT_ROOT/js; done
for F in $(ls -1 cgi-bin) ; do docker cp cgi-bin/$F $NAME:/var/www/localhost/cgi-bin ;done
docker cp JSON/PP.pm $NAME:/var/www/localhost/cgi-bin/JSON
docker cp YAML/Tiny.pm $NAME:/var/www/localhost/cgi-bin/YAML
for F in $(ls -1 cgi-bin) ; do docker exec $NAME chmod 775 /var/www/localhost/cgi-bin/$F ; done

