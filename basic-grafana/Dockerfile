# FROM alpine:3.9
FROM frolvlad/alpine-glibc:alpine-3.9_glibc-2.29
ENV TIMEZONE Europe/Paris

ENV GRAFANA_VERSION 5.0.4
# TODO: review if container can be upgraded to later grafana build
ENV GF_INSTALL_PLUGINS="simpod-json-datasource"

# Installing packages
RUN apk add --no-cache su-exec ca-certificates openssl
RUN apk add --no-cache --virtual .build-deps fontconfig

WORKDIR /scripts

ADD https://s3-us-west-2.amazonaws.com/grafana-releases/release/grafana-${GRAFANA_VERSION}.linux-x64.tar.gz ./
RUN addgroup grafana && \
    adduser -s /bin/false -G grafana -S -D grafana

COPY ./scripts/start.sh start.sh

RUN tar -C . -xzf grafana-$GRAFANA_VERSION.linux-x64.tar.gz && \
        mv grafana-${GRAFANA_VERSION} /grafana && \
        for D in dashboards data logs plugins; do  mkdir -p /var/lib/grafana/$D; done && \
        ln -s /grafana/plugins /var/lib/grafana/plugins && \
        mv /grafana/bin/* /usr/bin/ && \
        rm grafana-$GRAFANA_VERSION.linux-x64.tar.gz && \
        apk del .build-deps
RUN     grafana-cli plugins update-all && \
        rm -f /grafana/conf/*.ini

COPY ./files/defaults.ini /grafana/conf/

VOLUME  [ "/grafana" ]

EXPOSE 3000

ENTRYPOINT ["sh", "start.sh"]
