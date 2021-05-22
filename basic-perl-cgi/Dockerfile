# FROM nimmis/alpine-micro
FROM alpine:3.9


COPY root/. /

RUN apk update && apk upgrade && \
    apk add apache2 libxml2-dev apache2-utils apache-mod-fcgid perl perl-fcgi && \
    mkdir /var/run/mod_fcgid/ && chown -R apache.www-data /var/run/mod_fcgid  && \
    sed -i 's|AllowOverride [Nn]one|AllowOverride All|'     /etc/apache2/httpd.conf && \
    sed -i 's|#LoadModule cgi|LoadModule cgi|g'           /etc/apache2/httpd.conf && \
    sed -i 's|Options Indexes|Options |g'                 /etc/apache2/httpd.conf && \
    rm -rf /var/cache/apk/*

EXPOSE 80 443

# NOTE: infinity is notsupported option on alpine
# ENTRYPOINT ["sh", "-c", "/usr/sbin/httpd -k start; sleep infinity"]
ENTRYPOINT ["sh", "-c", "PIDFILE='/run/apache2/httpd.pid';/usr/sbin/httpd -k start; echo $? ; while [ ! -f /run/apache2/httpd.pid ] ; do echo 'wait for apache pid'; sleep 1; done; while true ; do pgrep -P $(cat /run/apache2/httpd.pid) > /dev/null; if [ $? != 0 ] ; then echo 'apache is gone'; exit 0; fi ; sleep 10; done" ]
