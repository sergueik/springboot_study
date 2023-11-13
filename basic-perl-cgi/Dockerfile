FROM alpine:3.9.5
user root
COPY root /

RUN apk --allow-untrusted update && apk upgrade && \
    apk add jq apache2 libxml2-dev apache2-utils apache-mod-fcgid perl perl-fcgi && \
    rm -rf /var/cache/apk/* && \
    mkdir /var/run/mod_fcgid/ && chown -R apache.www-data /var/run/mod_fcgid  && \
    sed -i 's|AllowOverride [Nn]one|AllowOverride All|' /etc/apache2/httpd.conf && \
    sed -i 's|#LoadModule cgi|LoadModule cgi|g' /etc/apache2/httpd.conf && \
    sed -i 's|#LoadModule headers_module|LoadModule headers_module|g' /etc/apache2/httpd.conf && \
    sed -i 's|Options Indexes|Options |g' /etc/apache2/httpd.conf && \
    grep -q 'Header set Access-Control-Allow-Origin' /etc/apache2/httpd.conf || sed -i 's|\(<Directory "/var/www/localhost/htdocs">\)|\1\nHeader set Access-Control-Allow-Origin "*"|' /etc/apache2/httpd.conf

    # NOTE: thw Perl command will update configuration in every run, since Perl run in sed mode. Can just use sed
    # perl -pi -e '$_ =~ s|(<Directory "/var/www/localhost/htdocs">)|$1\nHeader set Access-Control-Allow-Origin "*"|og unless /^\s*Header set Access-Control-Allow-Origin/' /etc/apache2/httpd.conf

COPY html /var/www/localhost/htdocs
COPY cgi-bin /var/www/localhost/cgi-bin
RUN sed -i 's|\r||g' /var/www/localhost/cgi-bin/*cgi
# COPY JSON /var/www/localhost/cgi-bin/JSON

# COPY YAML /var/www/localhost/cgi-bin/YAML
# COPY XML /var/www/localhost/cgi-bin/XML
# COPY URI /var/www/localhost/cgi-bin/XML
# COPY InfluxDB /var/www/localhost/cgi-bin/XML

RUN chmod 775 /var/www/localhost/cgi-bin/*cgi
EXPOSE 80 443
# NOTE: this is not shell scripting. 
# No need to escape '$' inside double-quoted sting

ENTRYPOINT ["sh", "-c", "PIDFILE='/run/apache2/httpd.pid';/usr/sbin/httpd -k start; echo $?; while [ ! -f $PIDFILE ] ; do echo 'wait for apache pid'; sleep 1; done; echo 'apache is running with ID '$(cat $PIDFILE); while true ; do pgrep -P $(cat $PIDFILE) > /dev/null; if [ $? != 0 ] ; then echo 'apache is gone'; exit 0; fi ; sleep 10; done" ]

