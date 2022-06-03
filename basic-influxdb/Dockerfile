# based on: https://github.com/container-examples/alpine-influxdb/blob/master/Dockerfile
FROM alpine:3.9

ENV TIMEZONE Europe/Paris
ENV INFLUXDB_VERSION 1.5.3

# Installing packages
RUN apk add --no-cache su-exec

# Work path
WORKDIR /scripts

# Downloading binaries
ADD https://dl.influxdata.com/influxdb/releases/influxdb-${INFLUXDB_VERSION}-static_linux_amd64.tar.gz ./
RUN addgroup influxdb && \
        adduser -s /bin/false -G influxdb -S -D influxdb

# Coping scripts
COPY ./scripts/start.sh start.sh

# Installing binaries
RUN tar -C . -xzf influxdb-${INFLUXDB_VERSION}-static_linux_amd64.tar.gz && \
        chmod +x influxdb-*/* && \
        cp -a influxdb-*/* /usr/bin/ && \
        rm -rf *.tar.gz* influxdb-*/

VOLUME [ "/var/lib/influxdb" ]

EXPOSE 8086

ENTRYPOINT [ "./start.sh" ]
