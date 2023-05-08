FROM alpine:3.9.5
RUN apk update && apk upgrade && \
    apk add jq curl && \
    rm -rf /var/cache/apk/* 

ENTRYPOINT ["/usr/bin/curl", "--silent", "--unix-socket", "/var/run/docker.sock", "http://v1.37/containers/json" ]
