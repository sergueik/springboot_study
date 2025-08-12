FROM alpine:3.9.5
user root

RUN apk --allow-untrusted update && apk upgrade && \
    apk add --no-cache apache2 && \
    rm -rf /var/cache/apk/* 

# Remove default index.html if any
RUN rm -f /var/www/localhost/htdocs/index.html

COPY html /var/www/localhost/htdocs
EXPOSE 80 443
# NOTE: this is not shell scripting. 
# No need to escape '$' inside double-quoted sting

# ENTRYPOINT ["sh", "-c", "PIDFILE='/run/apache2/httpd.pid';/usr/sbin/httpd -k start; echo $?; while [ ! -f $PIDFILE ] ; do echo 'wait for apache pid'; sleep 1; done; echo 'apache is running with ID '$(cat $PIDFILE); while true ; do pgrep -P $(cat $PIDFILE) > /dev/null; if [ $? != 0 ] ; then echo 'apache is gone'; exit 0; fi ; sleep 10; done" ]

# not needed for dummy
CMD ["httpd", "-D", "FOREGROUND"]
