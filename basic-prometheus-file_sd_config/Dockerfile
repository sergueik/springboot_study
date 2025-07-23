FROM golang:1.12.14-alpine3.9 as builder
user root
RUN apk --allow-untrusted update && apk upgrade && \
    apk add --no-cache go git libc-dev libc6-compat && \
    rm -rf /var/cache/apk/* 
ENV GO_VERSION=1.13.15
RUN wget https://golang.org/dl/go${GO_VERSION}.linux-amd64.tar.gz && \
    rm -rf /usr/local/go && \
    tar -C /usr/local -xzf go${GO_VERSION}.linux-amd64.tar.gz && \
    export PATH="/usr/local/go/bin:${PATH}" && \
    go version | grep -q "go${GO_VERSION}" || (echo "Go version mismatch, expected ${GO_VERSION}" >&2; exit 1) && \
    rm go${GO_VERSION}.linux-amd64.tar.gz

ENV PATH="/usr/local/go/bin:${PATH}"
# TODO: build custom JSON exporter here


FROM alpine:3.9.5
user root

RUN apk --allow-untrusted update && apk upgrade && \
    apk add --no-cache jq curl perl && \
    apk add --no-cache libc-dev libc6-compat && \
    rm -rf /var/cache/apk/* 

COPY lib /lib
COPY myapp.pl entrypoint.sh update_targets.sh /

RUN chmod +x entrypoint.sh update_targets.sh

# TODO: get a custom JSON exporter here

# COPY --from=builder /usr/local/bin/esbuild /usr/local/bin/esbuild

ENTRYPOINT ["sh", "-c", "PIDFILE='/run/app.pid'; perl myapp.pl daemon -l http://*:80; PID=$(ps ax|grep [p]erl|awk '{print $1}'); if [ ! -z $PID ] ; then echo $PID>$PIDFILE; fi; while [ ! -f $PIDFILE ] ; do echo 'wait for app pid'; sleep 1; done; echo 'app is running with ID '$(cat $PIDFILE); while true ; do pgrep -P $(cat $PIDFILE) > /dev/null; if [ $? != 0 ] ; then echo 'app is gone'; exit 0; fi ; sleep 10; done" ]
