FROM alpine:3.9.5
USER root

RUN apk --allow-untrusted update && apk upgrade && \
    apk add jq wget curl && \
    rm -rf /var/cache/apk/*

ENV JSON_EXPORTER_VERSION=0.7.0
RUN curl -sL -o  /tmp/json-exporter.tar.gz https://github.com/prometheus-community/json_exporter/releases/download/v${JSON_EXPORTER_VERSION}/json_exporter-${JSON_EXPORTER_VERSION}.linux-amd64.tar.gz  && \
    tar -C /tmp -xvf /tmp/json-exporter.tar.gz  && \
    mv /tmp/json_exporter-${JSON_EXPORTER_VERSION}.linux-amd64/json_exporter /usr/local/bin/  && \
    chmod +x /usr/local/bin/json_exporter

EXPOSE 7979 
ENTRYPOINT [ "json_exporter" , "--config.file=/json_exporter_config.yml", "--web.listen-address=:7979" ]
