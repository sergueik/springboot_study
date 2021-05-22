NAME=basic-perl-cgi-container
for D in css js ; do docker exec $NAME mkdir /var/www/localhost/htdocs/$D; done
for D in JSON YAML ; do docker exec $NAME mkdir /var/www/localhost/cgi-bin/$D; done
docker cp html/inventory.html $NAME:/var/www/localhost/htdocs
for F in $(ls -1 html/css) ; do docker cp html/css/$F $NAME:/var/www/localhost/htdocs/css; done
for F in $(ls -1 html/js) ; do docker cp html/js/$F $NAME:/var/www/localhost/htdocs/js; done
for F in $(ls -1 cgi-bin) ; do docker cp cgi-bin/$F $NAME:/var/www/localhost/cgi-bin ;done
docker cp JSON/PP.pm $NAME:/var/www/localhost/cgi-bin/JSON
docker cp YAML/Tiny.pm $NAME:/var/www/localhost/cgi-bin/YAML
for F in $(ls -1 cgi-bin) ; do docker exec $NAME chmod 775 /var/www/localhost/cgi-bin/$F ; done

