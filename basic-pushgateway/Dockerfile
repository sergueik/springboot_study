FROM alpine:3.9.5
RUN apk update && apk add wget
WORKDIR /
ARG VERSION='1.4.2'
ENV VERSION=$VERSION
RUN wget -q https://github.com/prometheus/pushgateway/releases/download/v$VERSION/pushgateway-$VERSION.linux-amd64.tar.gz \
&& tar xf /pushgateway-$VERSION.linux-amd64.tar.gz \
&& mv /pushgateway-$VERSION.linux-amd64/pushgateway /bin \
&& rm -rf /pushgateway-$VERSION.linux-amd64.tar.gz /pushgateway-$VERSION.linux-amd64
EXPOSE 9091
RUN mkdir -p /pushgateway && chown nobody:nobody /pushgateway
WORKDIR /pushgateway

USER 65534

ENTRYPOINT [ "/bin/pushgateway", "--persistence.file=/pushgateway/history.log" , "--persistence.interval=30s", "--log.format=json" ]
