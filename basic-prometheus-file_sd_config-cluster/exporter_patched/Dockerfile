# TODO
FROM alpine:3.9.5
user root

RUN apk --allow-untrusted update && apk upgrade && \
    apk add jq perl wget curl build-base perl-utils  && \
    rm -rf /var/cache/apk/*  && \
    curl -sL https://cpanmin.us |perl - App::cpanminus && \
    cpanm --no-interactive --notest Perl::Tidy



COPY lib /lib
COPY myapp.pl entrypoint.sh update_targets.sh json_exporter_config.yml prometheus.yml /
RUN sed -i 's/\r$//' entrypoint.sh update_targets.sh && \
    chmod +x entrypoint.sh update_targets.sh

RUN mkdir -p /etc/prometheus
# added dummy dynamic_targets.json
ENV JSON_EXPORTER_VERSION=0.7.0
RUN curl -sL -o  /tmp/json-exporter.tar.gz https://github.com/prometheus-community/json_exporter/releases/download/v${JSON_EXPORTER_VERSION}/json_exporter-${JSON_EXPORTER_VERSION}.linux-amd64.tar.gz  && \
    ls -l /tmp && \
    tar -C /tmp -xvf /tmp/json-exporter.tar.gz  && \
    mv /tmp/json_exporter-${JSON_EXPORTER_VERSION}.linux-amd64/json_exporter /usr/local/bin/  && \
    chmod +x /usr/local/bin/json_exporter

COPY dynamic_targets.json /etc/prometheus/
EXPOSE 80
ENTRYPOINT ["./entrypoint.sh"]
