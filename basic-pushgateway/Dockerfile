FROM alpine:3.9.5
WORKDIR /
ARG VERSION=1.4.2
ENV VERSION=$VERSION
# NOTE: wget exit code: 8 is likely a 404 server response
RUN apk update && apk add wget && \
    wget https://github.com/prometheus/pushgateway/releases/download/v$VERSION/pushgateway-$VERSION.linux-amd64.tar.gz && \
    tar xf pushgateway-$VERSION.linux-amd64.tar.gz && \
    mv pushgateway-$VERSION.linux-amd64/pushgateway /bin && \
    rm -rf pushgateway-$VERSION.linux-amd64.tar.gz pushgateway-$VERSION.linux-amd64 && \
   mkdir -p /pushgateway && chown nobody:nobody /pushgateway

WORKDIR /pushgateway
EXPOSE 9091

USER 65534

ENTRYPOINT [ "/bin/pushgateway", "--persistence.file=/pushgateway/history.log", "--persistence.interval=30s", "--log.format=json" ]
