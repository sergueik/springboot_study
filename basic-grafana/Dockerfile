FROM frolvlad/alpine-glibc:alpine-3.9_glibc-2.29
ENV TIMEZONE US/New York

ENV GRAFANA_VERSION 7.3.4

# NOTE: use "," as separator when installing mutiple plugins
ENV GF_INSTALL_PLUGINS="grafana-simple-json-datasource"

RUN apk add --no-cache su-exec ca-certificates openssl curl
RUN apk add --no-cache --virtual .build-deps fontconfig

WORKDIR /scripts

ADD https://dl.grafana.com/oss/release/grafana-${GRAFANA_VERSION}.linux-amd64.tar.gz ./
RUN addgroup grafana && \
    adduser -s /bin/false -G grafana -S -D grafana

COPY ./scripts/start.sh start.sh
RUN [ -z "$COMPOSE_CONVERT_WINDOWS_PATHS" ] && sed -i 's|\r||g' start.sh
RUN tar -C . -xzf grafana-$GRAFANA_VERSION.linux-amd64.tar.gz && \
        mv grafana-${GRAFANA_VERSION} /grafana && \
        for D in dashboards data logs plugins datasources; do mkdir -p /grafana/$D; done && \
        mkdir /var/lib/grafana && \
        for D in dashboards plugins datasources; do ln -s /grafana/$D /var/lib/grafana/$D; done  && \
        mv /grafana/bin/* /usr/bin/ && \
        rm grafana-$GRAFANA_VERSION.linux-amd64.tar.gz && \
        apk del .build-deps
RUN     grafana-cli plugins update-all && \
        rm -f /grafana/conf/*.ini

COPY ./files/defaults.ini /grafana/conf/
ADD mongodb-grafana /var/lib/grafana/plugins/grafana-mongodb-datasource
RUN test -f /var/lib/grafana/plugins/grafana-mongodb-datasource/package-lock.json && rm /var/lib/grafana/plugins/grafana-mongodb-datasource/package-lock.json
VOLUME  [ "/grafana" ]

EXPOSE 3000

ENTRYPOINT ["sh", "start.sh"]
