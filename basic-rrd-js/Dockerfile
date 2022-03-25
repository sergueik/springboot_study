FROM alpine:3.9.5

RUN apk update && apk upgrade && \
    apk add apache2 libxml2-dev apache2-utils apache-mod-fcgid && \
    rm -rf /var/cache/apk/* && \
    mkdir /var/run/mod_fcgid/ && chown -R apache.www-data /var/run/mod_fcgid  && \
    sed -i 's|AllowOverride [Nn]one|AllowOverride All|' /etc/apache2/httpd.conf && \
    sed -i 's|#LoadModule cgi|LoadModule cgi|g' /etc/apache2/httpd.conf && \
    sed -i 's|Options Indexes|Options |g' /etc/apache2/httpd.conf

COPY data /var/www/localhost/htdocs/data
COPY doc /var/www/localhost/htdocs/doc
COPY src /var/www/localhost/htdocs/src
COPY flot /var/www/localhost/htdocs/flot
# quick make accessible - 
RUN chmod -R 775 /var/www/localhost/htdocs && chown -R apache:apache /var/www/localhost/htdocs
EXPOSE 80 443
# NOTE: this is not shell scripting. 
# No need to escape '$' inside double-quoted sting

ENTRYPOINT ["sh", "-c", "PIDFILE='/run/apache2/httpd.pid';/usr/sbin/httpd -k start; echo $?; while [ ! -f $PIDFILE ] ; do echo 'wait for apache pid'; sleep 1; done; echo 'apache is running with ID '$(cat $PIDFILE); while true ; do pgrep -P $(cat $PIDFILE) > /dev/null; if [ $? != 0 ] ; then echo 'apache is gone'; exit 0; fi ; sleep 10; done" ]

