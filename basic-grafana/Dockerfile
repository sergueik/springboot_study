FROM frolvlad/alpine-glibc:alpine-3.9_glibc-2.29
ENV TIMEZONE US/New York

ENV GRAFANA_VERSION 5.0.4
# TODO: review if container can be upgraded to later grafana build
# NOTE: currently only one plugin at a time is supported 
# ENV GF_INSTALL_PLUGINS="simpod-json-datasource grafana-simple-json-datasource"
# ENV GF_INSTALL_PLUGINS="simpod-json-datasource"
ENV GF_INSTALL_PLUGINS="grafana-simple-json-datasource"
RUN apk add --no-cache su-exec ca-certificates openssl
RUN apk add --no-cache --virtual .build-deps fontconfig

WORKDIR /scripts
# TODO: can one pre-download the archive into workspace and copy cached file to save network bandwidth ?
COPY grafana-${GRAFANA_VERSION}.linux-x64.tar.gz /scripts
ADD https://s3-us-west-2.amazonaws.com/grafana-releases/release/grafana-${GRAFANA_VERSION}.linux-x64.tar.gz ./
RUN addgroup grafana && \
    adduser -s /bin/false -G grafana -S -D grafana

COPY ./scripts/start.sh start.sh

RUN tar -C . -xzf grafana-$GRAFANA_VERSION.linux-x64.tar.gz && \
        mv grafana-${GRAFANA_VERSION} /grafana && \
        for D in dashboards data logs plugins datasources; do mkdir -p /grafana/$D; done && \
        mkdir /var/lib/grafana && \
        for D in dashboards plugins datasources; do ln -s /grafana/$D /var/lib/grafana/$D; done  && \
        mv /grafana/bin/* /usr/bin/ && \
        rm grafana-$GRAFANA_VERSION.linux-x64.tar.gz && \
        apk del .build-deps
RUN     grafana-cli plugins update-all && \
        rm -f /grafana/conf/*.ini

COPY ./files/defaults.ini /grafana/conf/

VOLUME  [ "/grafana" ]

EXPOSE 3000

ENTRYPOINT ["sh", "start.sh"]
