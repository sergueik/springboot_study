FROM alpine:3.9.5
user root
ENV SCRIPT server.pl
RUN apk --allow-untrusted update && apk upgrade && \
    apk add jq curl perl && \
    rm -rf /var/cache/apk/* 
COPY lib /lib
COPY $SCRIPT /

ENTRYPOINT ["sh", "-c", "PIDFILE='/run/app.pid'; perl ${SCRIPT} daemon -l http://*:80; PID=$(ps ax|grep [p]erl|awk '{print $1}'); if [ ! -z $PID ] ; then echo $PID>$PIDFILE; fi; while [ ! -f $PIDFILE ] ; do echo 'wait for app pid'; sleep 1; done; echo 'app is running with ID '$(cat $PIDFILE); while true ; do pgrep -P $(cat $PIDFILE) > /dev/null; if [ $? != 0 ] ; then echo 'app is gone'; exit 0; fi ; sleep 10; done" ]
