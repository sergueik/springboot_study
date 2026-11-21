# FROM openjdk:8-jre-alpine3.9
# FROM eclipse-temurin:11-jre
FROM alpine/java:11-jre
ARG PUID=1000
ARG PGID=1000
#
RUN set -xe \
    && cat /etc/alpine-release \     
    && echo "http://dl-cdn.alpinelinux.org/alpine/v3.20/main" >> /etc/apk/repositories \
    && apk update \
    && apk add --no-cache --purge -uU \
        curl icu-libs unzip zlib-dev musl \
        mesa-gl \
        libreoffice libreoffice-base libreoffice-lang-uk \
        ttf-freefont ttf-opensans ttf-inconsolata font-awesome font-hack font-mononoki \
	ttf-liberation ttf-dejavu mesa-dri-gallium \
        libstdc++ dbus-x11 \
    && rm -rf /var/cache/apk/* /tmp/*
# TODO:
#   mesa-dri-swrast 
VOLUME /home/alpine/
#
USER root
RUN apk add shadow
RUN addgroup -g 1000 alpine 
RUN adduser -u 1000 -s /bin/sh -D -G alpine alpine
USER alpine
WORKDIR /home/alpine/
#
# ENTRYPOINT ["/usr/bin/libreoffice"]
ENTRYPOINT ["ash"]
# ENTRYPOINT ["/bin/bash"]
# ENTRYPOINT ["/init"]
